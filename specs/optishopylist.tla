--------------------------- MODULE optishopylist ---------------------------

EXTENDS TLC, Integers, FiniteSets, Sequences
CONSTANTS PRODUCTS, APPS, IDs, GATEAPP

PT == INSTANCE PT

set ++ item == set \union {item} 
set -- item == set \ {item}

(***************************************************************************)
(* In shoppying list, the product is in fact the identifier.               *)
(* Any item could have an information for how much of a product one        *)
(* wants to buy (not relevant in this specification).                      *)
(***************************************************************************)
ShopyItems == [id: PRODUCTS, bought: BOOLEAN]

ADD_ACTION == "add"
RM_ACTION == "rm"
SET_BOUGHT_ACTION == "set_bought"
REQ_SYNC_ACTION == "req_sync"
RESP_SYNC_ACTION == "resp_sync"
END_SYNC_ACTION == "end_sync"

(***************************************************************************)
(* Actions is the set of all possible actions in the system.               *)
(***************************************************************************)
Actions == {
    ADD_ACTION, 
    RM_ACTION, 
    SET_BOUGHT_ACTION, 
    REQ_SYNC_ACTION, 
    RESP_SYNC_ACTION,
    END_SYNC_ACTION
}

SyncActions == {REQ_SYNC_ACTION, RESP_SYNC_ACTION}

(***************************************************************************)
(* SyncMsgs is the set of all possible messages sent for synchronisation   *)
(* of shopy lists.                                                         *)
(***************************************************************************)
SyncMsgs ==
    [id: IDs,
     app: APPS, 
     list: SUBSET ShopyItems, 
     mergedList: SUBSET ShopyItems,
     type: SyncActions]

(***************************************************************************)
(* Messages sent for joining the network.                                  *)
(***************************************************************************)
JoinMsgs ==
    [app: APPS,
     knownHosts: SUBSET APPS,
     assignedGossipFriends: {apps \in (APPS \X APPS): apps[1] /= apps[2]}]

(***************************************************************************)
(* The spec now depicts a shopping-list app where the server app manages   *)
(* several users and hence multiple lists of items that synch eventually.  *)
(*                                                                         *)
(* The list contains unique items, thus we use a set.                      *)
(***************************************************************************)
(* --algorithm OptiShopyList

variable
    \* one shopping list for all APPS
    shopyList = [a \in APPS |-> {}],
    \* sync requests for all APPS
    syncReqQueue = [a \in APPS |-> <<>>],
    \* sync responses for all APPS
    syncRespQueue = [a \in APPS |-> <<>>],
    \* join to network requests
    joinReqQueue = <<>>,
    \* set of taken IDs
    takenIDs = {};

define
    (***********************************************************************)
    (* A couple of helpers for shopy-list items                            *)
    (***********************************************************************)
    
    NewShopyItem(list) == 
        [id     |-> (CHOOSE x \in PRODUCTS: ~\E i \in list: x = i.id), 
         bought |-> FALSE]
         
    ExistingShopyItem(list) == CHOOSE x \in list: TRUE
    
    ExistingNotBoughtShopyItem(list) == CHOOSE x \in list: x.bought = FALSE
    
    (***********************************************************************)
    (* Helpers for Sync messages request/response.                         *)
    (***********************************************************************)
    
    NewSyncMsg(id, a, l, ml, t) ==
        [id |-> id,
         app |-> a, 
         list |-> l, 
         mergedList |-> ml,
         type |-> t]
         
    NewSyncReqMsg(a, l, ml, t) ==
        NewSyncMsg(
            (CHOOSE i \in IDs: \A ti \in takenIDs: i = ti),
            a, l, ml, t
        )
        
    NewSyncReq(app) == 
        NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)
        
    NewSyncResp(app, mergeResult, id) == 
        NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)
    
    (***********************************************************************)
    (* Helpers for the decentralized network features.                     *)
    (***********************************************************************)
    IsGate(app) == app = GATEAPP
end define;

(***************************************************************************)
(* This proccess represents a shopy-list running                           *)
(* in one of the several network clients.                                  *)
(*                                                                         *)
(* Since Opti-shopylist is a decentralized program, the user creates a     *)
(* network of connected instances of Opti-shopylist.                       *)
(*                                                                         *)
(* We assume that every client app has only one shopy-list.                *)
(***************************************************************************)
fair process ClientApp \in APPS
variables
    gossipFriends = {},
    knownNetworkClients = {};

begin AppLoop:
    while TRUE do
        
        either
            \* JOIN NETWORK
            skip;
        or
            \* ADD
            await Cardinality(shopyList[self]) < Cardinality(PRODUCTS);
            shopyList[self] := shopyList[self] ++ NewShopyItem(shopyList[self]);
        or
            \* REMOVE
            await shopyList[self] /= {};
            shopyList[self] := shopyList[self] -- ExistingShopyItem(shopyList[self]);
        or
            \* set to BOUGHT
            await shopyList[self] /= {};
            await \E item \in shopyList[self]: ~item.bought;
            with modifiedItem = ExistingNotBoughtShopyItem(shopyList[self]) do
                shopyList[self] := shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE];
            end with;
        or
            \* SEND SYNC REQUEST
            with a \in (knownNetworkClients -- self), 
                 newRequest = NewSyncReq(self) do
                
                syncReqQueue[a] := Append(syncReqQueue[a], newRequest);
            end with;
        or
            \* RCV SYNC REQUEST
            await syncReqQueue[self] /= <<>>;
            with syncRequest = Head(syncReqQueue[self]),
                 mergeResult = shopyList[self] \union syncRequest.list,
                 newResp = NewSyncResp(self, mergeResult, syncRequest.id) do
                
                syncReqQueue[self] := Tail(syncReqQueue[self]);
                \* merge from request app
                shopyList[self] := mergeResult;
                syncRespQueue[syncRequest.app] := Append(syncRespQueue[syncRequest.app], newResp);
            end with;
        or
            \* RCV SYNC RESPONSE
            await syncRespQueue[self] /= <<>>;
            with syncResponse = Head(syncRespQueue[self]),
                 mergeResult = shopyList[self] \union syncResponse.list do
                
                shopyList[self] := mergeResult;
                syncRespQueue[self] := Tail(syncRespQueue[self]);
            end with;
        end either;
    end while;
end process;

end algorithm;
*)
\* BEGIN TRANSLATION (chksum(pcal) = "fd35c6b5" /\ chksum(tla) = "eea1508f")
VARIABLES shopyList, syncReqQueue, syncRespQueue, joinReqQueue, takenIDs

(* define statement *)
NewShopyItem(list) ==
    [id     |-> (CHOOSE x \in PRODUCTS: ~\E i \in list: x = i.id),
     bought |-> FALSE]

ExistingShopyItem(list) == CHOOSE x \in list: TRUE

ExistingNotBoughtShopyItem(list) == CHOOSE x \in list: x.bought = FALSE





NewSyncMsg(id, a, l, ml, t) ==
    [id |-> id,
     app |-> a,
     list |-> l,
     mergedList |-> ml,
     type |-> t]

NewSyncReqMsg(a, l, ml, t) ==
    NewSyncMsg(
        (CHOOSE i \in IDs: \A ti \in takenIDs: i = ti),
        a, l, ml, t
    )

NewSyncReq(app) ==
    NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)

NewSyncResp(app, mergeResult, id) ==
    NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)




IsGate(app) == app = GATEAPP

VARIABLES gossipFriends, knownNetworkClients

vars == << shopyList, syncReqQueue, syncRespQueue, joinReqQueue, takenIDs, 
           gossipFriends, knownNetworkClients >>

ProcSet == (APPS)

Init == (* Global variables *)
        /\ shopyList = [a \in APPS |-> {}]
        /\ syncReqQueue = [a \in APPS |-> <<>>]
        /\ syncRespQueue = [a \in APPS |-> <<>>]
        /\ joinReqQueue = <<>>
        /\ takenIDs = {}
        (* Process ClientApp *)
        /\ gossipFriends = [self \in APPS |-> {}]
        /\ knownNetworkClients = [self \in APPS |-> {}]

ClientApp(self) == /\ \/ /\ TRUE
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue>>
                      \/ /\ Cardinality(shopyList[self]) < Cardinality(PRODUCTS)
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] ++ NewShopyItem(shopyList[self])]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue>>
                      \/ /\ shopyList[self] /= {}
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- ExistingShopyItem(shopyList[self])]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue>>
                      \/ /\ shopyList[self] /= {}
                         /\ \E item \in shopyList[self]: ~item.bought
                         /\ LET modifiedItem == ExistingNotBoughtShopyItem(shopyList[self]) IN
                              shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE]]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue>>
                      \/ /\ \E a \in (knownNetworkClients[self] -- self):
                              LET newRequest == NewSyncReq(self) IN
                                syncReqQueue' = [syncReqQueue EXCEPT ![a] = Append(syncReqQueue[a], newRequest)]
                         /\ UNCHANGED <<shopyList, syncRespQueue>>
                      \/ /\ syncReqQueue[self] /= <<>>
                         /\ LET syncRequest == Head(syncReqQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncRequest.list IN
                                LET newResp == NewSyncResp(self, mergeResult, syncRequest.id) IN
                                  /\ syncReqQueue' = [syncReqQueue EXCEPT ![self] = Tail(syncReqQueue[self])]
                                  /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                  /\ syncRespQueue' = [syncRespQueue EXCEPT ![syncRequest.app] = Append(syncRespQueue[syncRequest.app], newResp)]
                      \/ /\ syncRespQueue[self] /= <<>>
                         /\ LET syncResponse == Head(syncRespQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncResponse.list IN
                                /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                /\ syncRespQueue' = [syncRespQueue EXCEPT ![self] = Tail(syncRespQueue[self])]
                         /\ UNCHANGED syncReqQueue
                   /\ UNCHANGED << joinReqQueue, takenIDs, gossipFriends, 
                                   knownNetworkClients >>

Next == (\E self \in APPS: ClientApp(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in APPS : WF_vars(ClientApp(self))

\* END TRANSLATION 

TypeOK ==
    /\ \A a \in APPS: 
        /\ shopyList[a] \subseteq ShopyItems
        /\ PT!Range(syncReqQueue[a]) \subseteq SyncMsgs
        /\ PT!Range(syncRespQueue[a]) \subseteq SyncMsgs
    /\ PT!Range(joinReqQueue) \subseteq JoinMsgs
    /\ knownNetworkClients \subseteq APPS
    /\ gossipFriends \subseteq APPS
    /\ takenIDs \subseteq IDs

=============================================================================
\* Modification History
\* Last modified Sun Mar 07 23:12:31 CET 2021 by davd
\* Created Tue Mar 02 12:33:43 CET 2021 by davd
