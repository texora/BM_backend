import Cycles "mo:base/ExperimentalCycles";
import Principal "mo:base/Principal";
import Buffer "mo:base/Buffer";
import Nat8 "mo:base/Nat8";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Int "mo:base/Int";
import Nat64 "mo:base/Nat64";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Text "mo:base/Text";
import Float "mo:base/Float";
import Int64 "mo:base/Int64";
import Interface "ic-management-interface";
import Types "types";
import ICPLedger "icp_ledger";
import SHA224 "SHA224";
import CRC32 "CRC32";
actor class X() = this {

  let NNS_CYCLES_MINTING_CANISTER_ID = "rkp4c-7iaaa-aaaaa-aaaca-cai";
  let NNS_ICP_LEDGER_CANISTER_ID = "ryjl3-tyaaa-aaaaa-aaaba-cai";

  let CMC = actor (NNS_CYCLES_MINTING_CANISTER_ID) : Types.Self;
  let ICP_LEDGER = actor (NNS_ICP_LEDGER_CANISTER_ID) : ICPLedger.Self;

  public type ConvertExactICP2CyclesFromNNSCMCResponseType = Types.ConvertExactICP2CyclesFromNNSCMCResponseType;
  public type AccountIdentifier = Blob;
  public type Subaccount = Blob;
  public type IcpXdrConversionRateResponse = Types.IcpXdrConversionRateResponse;

  public func add_cycles(canister_id : Text, cycles : Nat) : async Text {
    let IC = "aaaaa-aa";
    let ic = actor (IC) : Interface.Self;
    Cycles.add<system>(cycles);
    await ic.deposit_cycles({ canister_id = Principal.fromText(canister_id) });
    return "sucess";
  };

  public func icp_balance() : async Nat {
    await ICP_LEDGER.icrc1_balance_of({
      owner = Principal.fromActor(this);
      subaccount = null;
    });
  };
  func beBytes(n : Nat32) : [Nat8] {
    func byte(n : Nat32) : Nat8 {
      Nat8.fromNat(Nat32.toNat(n & 0xff));
    };
    [byte(n >> 24), byte(n >> 16), byte(n >> 8), byte(n)];
  };

  public func accountIdentifier(principal : Principal, subaccount : Subaccount) : async AccountIdentifier {
    let hash = SHA224.Digest();
    hash.write([0x0A]);
    hash.write(Blob.toArray(Text.encodeUtf8("account-id")));
    hash.write(Blob.toArray(Principal.toBlob(principal)));
    hash.write(Blob.toArray(subaccount));
    let hashSum = hash.sum();
    let crc32Bytes = beBytes(CRC32.ofArray(hashSum));
    Blob.fromArray(Array.append(crc32Bytes, hashSum));
  };

  public shared func get_icp_xdr_conversion_rate() : async IcpXdrConversionRateResponse {
    await CMC.get_icp_xdr_conversion_rate();
  };

  public func top_up_canisters(params : [{ minerId : Text; topUpAmount : Nat }]) : async [?ConvertExactICP2CyclesFromNNSCMCResponseType] {
    let paramSize = Array.size(params);
    if (paramSize == 0) { return [] };
    let rate = await get_icp_xdr_conversion_rate();
    let cycles_per_xdr : Nat = 1_000_000_000_000;
    var total = 0 : Nat;
    for (param in params.vals()) {
      total := param.topUpAmount + total;
    };
    let e8ICP = await e8ICPForCycles(total * cycles_per_xdr, rate);
    let balance = await ICP_LEDGER.icrc1_balance_of({
      owner = Principal.fromActor(this);
      subaccount = null;
    });
    assert balance >= (Nat64.toNat(e8ICP) + 10000 * paramSize);
    let mintCyclesResponseArray = Array.init<async ?ConvertExactICP2CyclesFromNNSCMCResponseType>(paramSize, async null);
    let mintCyclesResultArray = Array.init<?ConvertExactICP2CyclesFromNNSCMCResponseType>(paramSize, null);
    var index = 0;
    for (param in params.vals()) {
      let e8ICP = await e8ICPForCycles(param.topUpAmount * cycles_per_xdr, rate);
      mintCyclesResponseArray[index] := mintCycles({
        e8ICP = Nat64.toNat(e8ICP);
        recipientCanisterId = Principal.fromText(param.minerId);
      });
      index := index + 1;
    };
    index := 0;
    for (resp in mintCyclesResponseArray.vals()) {
      mintCyclesResultArray[index] := await resp;
      index := index + 1;
    };
    return Array.freeze(mintCyclesResultArray);
  };

  public func e8ICPForCycles(cycles : Nat, response : IcpXdrConversionRateResponse) : async Nat64 {

    let { data } = response;
    let { xdr_permyriad_per_icp } = data;

    let cycles_per_xdr : Nat64 = 1_000_000_000_000;
    let icp_per_xdr_permyriad_float = (10_000 : Float) / (Float.fromInt(Nat64.toNat(xdr_permyriad_per_icp) * Nat64.toNat(cycles_per_xdr)));

    let E8S_PER_ICP = 100_000_000;
    let e8ICP_float = icp_per_xdr_permyriad_float * Float.fromInt(cycles) * Float.fromInt(E8S_PER_ICP);

    return Int64.toNat64(Float.toInt64(e8ICP_float));
  };

  public func mintCycles({ e8ICP : Nat; recipientCanisterId : Principal }) : async ?ConvertExactICP2CyclesFromNNSCMCResponseType {

    let logBuffer = Buffer.Buffer<Text>(0);
    var conversionSuccessful = false;

    let cygnusCycleBalanceBeforeConversion = Cycles.balance();
    logBuffer.add(">>>>> Cygnus Cycle balance before conversion: " # debug_show (cygnusCycleBalanceBeforeConversion) # " <<<<<");
    let TOP_UP_CANISTER_MEMO = 0x50555054 : Nat64;

    let FEE = 10000 : Nat64;
    let to_subaccount = await principalToSubAccount(recipientCanisterId);
    let account = {
      owner = Principal.fromText(NNS_CYCLES_MINTING_CANISTER_ID);
      subaccount = Blob.fromArray(to_subaccount);
    };
    let result = await ICP_LEDGER.transfer({
      to = await accountIdentifier(account.owner, account.subaccount);
      fee = { e8s = FEE };
      memo = TOP_UP_CANISTER_MEMO;
      from_subaccount = null;
      amount = { e8s = Nat64.fromNat(e8ICP) };
      created_at_time = null;
    });

    switch (result) {
      case (#Ok(blockIndex)) {
        logBuffer.add(">>>>> Transfer successful. Block index: " # debug_show (blockIndex) # " <<<<<");
        let notifyMintCyclesResponse = await CMC.notify_top_up({
          block_index = (blockIndex);
          canister_id = recipientCanisterId;
        });
        switch (notifyMintCyclesResponse) {
          case (#Ok(notifyMintCyclesSuccess)) {
            logBuffer.add(">>>>> Notify mint cycles " # debug_show (notifyMintCyclesSuccess));
            conversionSuccessful := true;
          };
          case (#Err(notifyError)) {
            logBuffer.add(">>>>> Notify mint cycles failed. Reason: " # debug_show (notifyError) # " <<<<<");
            ignore switch (notifyError) {
              case (#Refunded(particulars)) {
                logBuffer.add(("error_type: Refunded"));
                logBuffer.add(("block_index: " # debug_show (particulars.block_index)));
                logBuffer.add(("reason: " # debug_show (particulars.reason)));
              };
              case (#InvalidTransaction(particulars)) {
                logBuffer.add(("error_type: InvalidTransaction"));
                logBuffer.add(("reason: " # debug_show (particulars)));
              };
              case (#Other(particulars)) {
                logBuffer.add(("error_type: Other"));
                logBuffer.add(("error_message: " # debug_show (particulars.error_message)));
                logBuffer.add(("error_code: " # debug_show (particulars.error_code)));
              };
              case (#Processing()) {
                logBuffer.add(("error_type: Processing"));
              };
              case (#TransactionTooOld(particulars)) {
                logBuffer.add(("error_type: TransactionTooOld"));
                logBuffer.add(("block_index: " # debug_show (particulars)));
              };
            };
          };
        };
      };
      case (#Err(transferError)) {
        ignore switch (transferError) {
          case (#InsufficientFunds(particulars)) {
            logBuffer.add(("error_type: InsufficientFunds"));
            logBuffer.add(("balance: " # debug_show (particulars.balance)));
          };
          case (#BadFee(particulars)) {
            logBuffer.add(("error_type: BadFee"));
            logBuffer.add(("expected_fee: " # debug_show (particulars.expected_fee)));
          };
          case (#TxCreatedInFuture()) {
            logBuffer.add(("error_type: TxCreatedInFuture"));
          };
          case (#TxDuplicate(particulars)) {
            logBuffer.add(("error_type: TxDuplicate"));
            logBuffer.add(("duplicate_of: " # debug_show (particulars.duplicate_of)));
          };
          case (#TxTooOld(particulars)) {
            logBuffer.add(("error_type: TxTooOld"));
            logBuffer.add(("allowed_window_nanos: " # debug_show (particulars.allowed_window_nanos)));
          };
          case (#GenericError(particulars)) {
            logBuffer.add(("error_type: GenericError"));
            logBuffer.add(("message: " # debug_show (particulars.message)));
            logBuffer.add(("error_code: " # debug_show (particulars.error_code)));
          };
          case (#TemporarilyUnavailable()) {
            logBuffer.add(("error_type: TemporarilyUnavailable"));
          };
          case (#CreatedInFuture(particulars)) {
            logBuffer.add(("error_type: CreatedInFuture"));
            logBuffer.add(("ledger_time: " # debug_show (particulars.ledger_time)));
          };
          case (#TooOld()) {
            logBuffer.add(("error_type: TooOld"));
          };
          case (#Duplicate(particulars)) {
            logBuffer.add(("error_type: Duplicate"));
            logBuffer.add(("duplicate_of: " # debug_show (particulars.duplicate_of)));
          };
          case (#BadBurn(particulars)) {
            logBuffer.add(("error_type: BadBurn"));
            logBuffer.add(("min_burn_amount: " # debug_show (particulars.min_burn_amount)));
          };
        };
      };
    };

    let cygnusCycleBalanceAfterConversion = Cycles.balance();
    logBuffer.add(">>>>> Cygnus Cycle balance after conversion: " # debug_show (Cycles.balance()) # " <<<<<");

    let cygnusCycleBalanceAbsoluteDifference = Int.abs(cygnusCycleBalanceAfterConversion - cygnusCycleBalanceBeforeConversion);

    return ?{
      logs = Buffer.toArray(logBuffer);
      cygnusCycleBalanceBeforeConversion;
      cygnusCycleBalanceAfterConversion;
      cygnusCycleBalanceAbsoluteDifference;
      conversionSuccessful;
    };
  };

  public shared func principalToSubAccount(id : Principal) : async [Nat8] {
    let p = Blob.toArray(Principal.toBlob(id));
    Array.tabulate(
      32,
      func(i : Nat) : Nat8 {
        if (i >= p.size() + 1) 0 else if (i == 0) (Nat8.fromNat(p.size())) else (p[i - 1]);
      },
    );
  };
};
