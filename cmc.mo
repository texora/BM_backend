import Float "mo:base/Float";
import Nat64 "mo:base/Nat64";
import Int64 "mo:base/Int64";
import ICPLedger "icp";
import Buffer "mo:base/Buffer";
import Cycles "mo:base/ExperimentalCycles";
import Int "mo:base/Int";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Account "icPCH/Account";
import Hex "icPCH/Hex";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Utils "../utility/utils";

module {

    public type AccountIdentifier = Text;
    public type BlockIndex = Nat64;
    public type CanisterSettings = {
        freezing_threshold : ?Nat;
        wasm_memory_threshold : ?Nat;
        controllers : ?[Principal];
        reserved_cycles_limit : ?Nat;
        log_visibility : ?log_visibility;
        wasm_memory_limit : ?Nat;
        memory_allocation : ?Nat;
        compute_allocation : ?Nat;
    };
    public type CreateCanisterArg = {
        subnet_selection : ?SubnetSelection;
        settings : ?CanisterSettings;
        subnet_type : ?Text;
    };
    public type CreateCanisterError = {
        #Refunded : { create_error : Text; refund_amount : Nat };
    };
    public type CreateCanisterResult = {
        #Ok : Principal;
        #Err : CreateCanisterError;
    };
    public type Cycles = Nat;
    public type CyclesCanisterInitPayload = {
        exchange_rate_canister : ?ExchangeRateCanister;
        cycles_ledger_canister_id : ?Principal;
        last_purged_notification : ?Nat64;
        governance_canister_id : ?Principal;
        minting_account_id : ?AccountIdentifier;
        ledger_canister_id : ?Principal;
    };
    public type ExchangeRateCanister = { #Set : Principal; #Unset };
    public type IcpXdrConversionRate = {
        xdr_permyriad_per_icp : Nat64;
        timestamp_seconds : Nat64;
    };
    public type IcpXdrConversionRateResponse = {
        certificate : Blob;
        data : IcpXdrConversionRate;
        hash_tree : Blob;
    };
    public type Memo = ?Blob;
    public type NotifyCreateCanisterArg = {
        controller : Principal;
        block_index : BlockIndex;
        subnet_selection : ?SubnetSelection;
        settings : ?CanisterSettings;
        subnet_type : ?Text;
    };
    public type NotifyCreateCanisterResult = {
        #Ok : Principal;
        #Err : NotifyError;
    };
    public type NotifyError = {
        #Refunded : { block_index : ?BlockIndex; reason : Text };
        #InvalidTransaction : Text;
        #Other : { error_message : Text; error_code : Nat64 };
        #Processing;
        #TransactionTooOld : BlockIndex;
    };
    public type NotifyMintCyclesArg = {
        block_index : BlockIndex;
        deposit_memo : Memo;
        to_subaccount : Subaccount;
    };
    public type NotifyMintCyclesResult = {
        #Ok : NotifyMintCyclesSuccess;
        #Err : NotifyError;
    };
    public type NotifyMintCyclesSuccess = {
        balance : Nat;
        block_index : Nat;
        minted : Nat;
    };
    public type NotifyTopUpArg = {
        block_index : BlockIndex;
        canister_id : Principal;
    };
    public type NotifyTopUpResult = { #Ok : Cycles; #Err : NotifyError };
    public type PrincipalsAuthorizedToCreateCanistersToSubnetsResponse = {
        data : [(Principal, [Principal])];
    };
    public type Subaccount = ?Blob;
    public type SubnetFilter = { subnet_type : ?Text };
    public type SubnetSelection = {
        #Filter : SubnetFilter;
        #Subnet : { subnet : Principal };
    };
    public type SubnetTypesToSubnetsResponse = { data : [(Text, [Principal])] };
    public type log_visibility = { #controllers; #public_ };
    public type Self = actor {
        create_canister : shared CreateCanisterArg -> async CreateCanisterResult;
        get_build_metadata : shared query () -> async Text;
        get_icp_xdr_conversion_rate : shared query () -> async IcpXdrConversionRateResponse;
        get_principals_authorized_to_create_canisters_to_subnets : shared query () -> async PrincipalsAuthorizedToCreateCanistersToSubnetsResponse;
        get_subnet_types_to_subnets : shared query () -> async SubnetTypesToSubnetsResponse;
        notify_create_canister : shared NotifyCreateCanisterArg -> async NotifyCreateCanisterResult;
        notify_mint_cycles : shared NotifyMintCyclesArg -> async NotifyMintCyclesResult;
        notify_top_up : shared NotifyTopUpArg -> async NotifyTopUpResult;
    };

    public let NNS_CYCLES_MINTING_CANISTER_ID = "rkp4c-7iaaa-aaaaa-aaaca-cai";
    public let NNS_ICP_LEDGER_CANISTER_ID = "ryjl3-tyaaa-aaaaa-aaaba-cai";

    let CMC = actor (NNS_CYCLES_MINTING_CANISTER_ID) : Self;
    let ICP_LEDGER = actor (NNS_ICP_LEDGER_CANISTER_ID) : ICPLedger.Self;

    public func e8ICPForCycles({ cycles : Nat }) : async Nat64 {

        let { data } = await CMC.get_icp_xdr_conversion_rate();
        let { xdr_permyriad_per_icp } = data;

        let cycles_per_xdr : Nat64 = 1_000_000_000_000;
        let icp_per_xdr_permyriad_float = (10_000 : Float) / (Float.fromInt(Nat64.toNat(xdr_permyriad_per_icp) * Nat64.toNat(cycles_per_xdr)));

        let E8S_PER_ICP = 100_000_000;
        let e8ICP_float = icp_per_xdr_permyriad_float * Float.fromInt(cycles) * Float.fromInt(E8S_PER_ICP);

        return Int64.toNat64(Float.toInt64(e8ICP_float));
    };

    public type ConvertExactICP2CyclesFromNNSCMCResponseType = {
        logs : [Text];
        cygnusCycleBalanceAfterConversion : Nat;
        cygnusCycleBalanceBeforeConversion : Nat;
        cygnusCycleBalanceAbsoluteDifference : Nat;
        conversionSuccessful : Bool;
    };

    public func principalToSubAccount(id : Principal) : [Nat8] {
        let p = Blob.toArray(Principal.toBlob(id));
        Array.tabulate(
            32,
            func(i : Nat) : Nat8 {
                if (i >= p.size() + 1) 0 else if (i == 0) (Nat8.fromNat(p.size())) else (p[i - 1]);
            },
        );
    };

    public func mintCycles({ e8ICP : Nat; recipientCanisterId : Principal }) : async ConvertExactICP2CyclesFromNNSCMCResponseType {

        let logBuffer = Buffer.Buffer<Text>(0);
        var conversionSuccessful = false;

        let cygnusCycleBalanceBeforeConversion = Cycles.balance();
        logBuffer.add(">>>>> Cygnus Cycle balance before conversion: " # debug_show (cygnusCycleBalanceBeforeConversion) # " <<<<<");

        let TOP_UP_CANISTER_MEMO = 0x50555054 : Nat64;

        let FEE = 10000 : Nat64;
        let to_subaccount = principalToSubAccount(recipientCanisterId);
        let account = Account.accountIdentifier(Principal.fromText(NNS_CYCLES_MINTING_CANISTER_ID), Blob.fromArray(to_subaccount));
        let result = await ICP_LEDGER.transfer({
            to = account;
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
                    block_index = blockIndex;
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

        return {
            logs = Buffer.toArray(logBuffer);
            cygnusCycleBalanceBeforeConversion;
            cygnusCycleBalanceAfterConversion;
            cygnusCycleBalanceAbsoluteDifference;
            conversionSuccessful;
        };
    };

};
