--------------------------- MODULE optishopylist ---------------------------

EXTENDS TLC, Integers, FiniteSets, Sequences
CONSTANTS ITEM_IDs, APPS, IDs

PT == INSTANCE PT

set ++ item == set \union {item} 
set -- item == set \ {item}

(***************************************************************************)
(* In shoppying list, the product is in fact the identifier.               *)
(* Any item could have an information for how much of a product one        *)
(* wants to buy (not relevant in this specification).                      *)
(***************************************************************************)
ShopyItems == [id: ITEM_IDs, bought: BOOLEAN]

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
    \* set of taken IDs
    takenIDs = {};

define
    NewShopyItem(list) == 
        [id     |-> (CHOOSE x \in ITEM_IDs: ~\E i \in list: x = i.id), 
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
            (CHOOSE i \in IDs: ~\E ti \in takenIDs: i = ti),
            a, l, ml, t
        )
    NewSyncReq(app) == 
        NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)
    NewSyncResp(app, mergeResult, id) == 
        NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)
end define;

(***************************************************************************)
(* This proccess represents a shopy-list running                           *)
(* in one of the several offline clients.                                  *)
(* Several shopy-lists are running on various app processes. Actually,     *)
(* we specify what happens in the system when 2 client apps want to synch  *)
(* their shopy-list. We assume that every client app has only one such     *)
(* list since the user is able to synch any 2 lists together.              *)
(***************************************************************************)
fair process AppLoop \in APPS
variables
    actionHistory = <<>>;

begin AppLoop:
    while TRUE do
        
        either 
            \* ADD
            await Cardinality(shopyList[self]) < Cardinality(ITEM_IDs);
            shopyList[self] := shopyList[self] ++ NewShopyItem(shopyList[self]);
            actionHistory := Append(actionHistory, ADD_ACTION);
        or
            \* REMOVE
            await shopyList[self] /= {};
            shopyList[self] := shopyList[self] -- ExistingShopyItem(shopyList[self]);
            actionHistory := Append(actionHistory, RM_ACTION);
        or
            \* set to BOUGHT
            await shopyList[self] /= {};
            await \E item \in shopyList[self]: ~item.bought;
            with modifiedItem = ExistingNotBoughtShopyItem(shopyList[self]) do
                shopyList[self] := shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE];
                actionHistory := Append(actionHistory, SET_BOUGHT_ACTION);
            end with;
        or
            \* request a sync with another app
            with a \in (APPS -- self) do
                syncReqQueue[a] := Append(syncReqQueue[a], NewSyncReq(a));
                actionHistory := Append(actionHistory, REQ_SYNC_ACTION);
            end with;
        or
            \* receive a sync request from another app
            await syncReqQueue[self] /= <<>>;
            with syncRequest = Head(syncReqQueue[self]),
                 mergeResult = shopyList[self] \union syncRequest.list do
                \* merge from request app
                shopyList[self] := mergeResult;
                syncRespQueue[syncRequest.app] := Append(syncRespQueue[syncRequest.app], NewSyncResp(self, mergeResult, syncRequest.id));
                actionHistory := Append(actionHistory, RESP_SYNC_ACTION);
                syncReqQueue[self] := Tail(syncReqQueue[self]);
                takenIDs := takenIDs -- syncRequest.id;
            end with;
        or
            \* receive a sync response
            await syncRespQueue[self] /= <<>>;
            with syncResponse = Head(syncRespQueue[self]),
                 mergeResult = shopyList[self] \union syncResponse.list do
                shopyList[self] := mergeResult;
                actionHistory := Append(actionHistory, END_SYNC_ACTION);
                syncRespQueue[self] := Tail(syncRespQueue[self]);
                takenIDs := takenIDs -- syncResponse.id;
            end with;
        end either;
    end while;
end process;

end algorithm;
*)
\* BEGIN TRANSLATION (chksum(pcal) = "f33e3231" /\ chksum(tla) = "653f15e1")
\* Label AppLoop of process AppLoop at line 100 col 5 changed to AppLoop_
VARIABLES shopyList, syncReqQueue, syncRespQueue, takenIDs

(* define statement *)
NewShopyItem(list) ==
    [id     |-> (CHOOSE x \in ITEM_IDs: ~\E i \in list: x = i.id),
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
        (CHOOSE i \in IDs: ~\E ti \in takenIDs: i = ti),
        a, l, ml, t
    )
NewSyncReq(app) ==
    NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)
NewSyncResp(app, mergeResult, id) ==
    NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)

VARIABLE actionHistory

vars == << shopyList, syncReqQueue, syncRespQueue, takenIDs, actionHistory >>

ProcSet == (APPS)

Init == (* Global variables *)
        /\ shopyList = [a \in APPS |-> {}]
        /\ syncReqQueue = [a \in APPS |-> <<>>]
        /\ syncRespQueue = [a \in APPS |-> <<>>]
        /\ takenIDs = {}
        (* Process AppLoop *)
        /\ actionHistory = [self \in APPS |-> <<>>]

AppLoop(self) == \/ /\ Cardinality(shopyList[self]) < Cardinality(ITEM_IDs)
                    /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] ++ NewShopyItem(shopyList[self])]
                    /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], ADD_ACTION)]
                    /\ UNCHANGED <<syncReqQueue, syncRespQueue, takenIDs>>
                 \/ /\ shopyList[self] /= {}
                    /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- ExistingShopyItem(shopyList[self])]
                    /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], RM_ACTION)]
                    /\ UNCHANGED <<syncReqQueue, syncRespQueue, takenIDs>>
                 \/ /\ shopyList[self] /= {}
                    /\ \E item \in shopyList[self]: ~item.bought
                    /\ LET modifiedItem == ExistingNotBoughtShopyItem(shopyList[self]) IN
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE]]
                         /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], SET_BOUGHT_ACTION)]
                    /\ UNCHANGED <<syncReqQueue, syncRespQueue, takenIDs>>
                 \/ /\ \E a \in (APPS -- self):
                         /\ syncReqQueue' = [syncReqQueue EXCEPT ![a] = Append(syncReqQueue[a], NewSyncReq(a))]
                         /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], REQ_SYNC_ACTION)]
                    /\ UNCHANGED <<shopyList, syncRespQueue, takenIDs>>
                 \/ /\ syncReqQueue[self] /= <<>>
                    /\ LET syncRequest == Head(syncReqQueue[self]) IN
                         LET mergeResult == shopyList[self] \union syncRequest.list IN
                           /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                           /\ syncRespQueue' = [syncRespQueue EXCEPT ![syncRequest.app] = Append(syncRespQueue[syncRequest.app], NewSyncResp(self, mergeResult, syncRequest.id))]
                           /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], RESP_SYNC_ACTION)]
                           /\ syncReqQueue' = [syncReqQueue EXCEPT ![self] = Tail(syncReqQueue[self])]
                           /\ takenIDs' = takenIDs -- syncRequest.id
                 \/ /\ syncRespQueue[self] /= <<>>
                    /\ LET syncResponse == Head(syncRespQueue[self]) IN
                         LET mergeResult == shopyList[self] \union syncResponse.list IN
                           /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                           /\ actionHistory' = [actionHistory EXCEPT ![self] = Append(actionHistory[self], END_SYNC_ACTION)]
                           /\ syncRespQueue' = [syncRespQueue EXCEPT ![self] = Tail(syncRespQueue[self])]
                           /\ takenIDs' = takenIDs -- syncResponse.id
                    /\ UNCHANGED syncReqQueue

Next == (\E self \in APPS: AppLoop(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in APPS : WF_vars(AppLoop(self))

\* END TRANSLATION 

TypeOK ==
    /\ \A a \in APPS: 
        /\ PT!Range(actionHistory[a]) \subseteq Actions
        /\ shopyList[a] \subseteq ShopyItems
        /\ PT!Range(syncReqQueue[a]) \subseteq SyncMsgs
        /\ PT!Range(syncRespQueue[a]) \subseteq SyncMsgs
    /\ takenIDs \subseteq IDs

SyncReqResp ==
    \E id \in IDs:
        (\E a \in APPS: \E req \in PT!Range(actionHistory[a]):
            /\ req \notin Actions
            /\ req \in SyncMsgs
            /\ id = req.id
            /\ REQ_SYNC_ACTION = req.type)
        =>
        <>[](\E a \in APPS: \E resp \in PT!Range(actionHistory[a]):
            /\ resp \notin Actions
            /\ resp \in SyncMsgs
            /\ id = resp.id
            /\ END_SYNC_ACTION = resp.type)

=============================================================================
\* Modification History
\* Last modified Sat Mar 06 20:48:55 CET 2021 by davd
\* Created Tue Mar 02 12:33:43 CET 2021 by davd
