%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(git_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("catalog.hrl").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=git_inventory(),
    ok=git_appl(),
    ok=start_appls(),
    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_appls()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    %% Set up two nodes
    NodeName1="node1",
    NodeName2="node2",
    HostId=net_adm:localhost(),
    Node1=list_to_atom("node1"++"@"++HostId),
    Node2=list_to_atom("node2"++"@"++HostId),
    Cookie=atom_to_list(erlang:get_cookie()),
    BasicArgs=" -setcookie "++Cookie,
    
    rpc:call(Node1,init,stop,[]),
    rpc:call(Node2,init,stop,[]),
  
    {ok,Node1}=slave:start(HostId,NodeName1,BasicArgs),
    {ok,Node2}=slave:start(HostId,NodeName2,BasicArgs),

    {ok,InventoryList}=catalog:get_inventory(),

    %% Node1 adder
    [{ok,RdPaths}]=[catalog:get_paths("resource_discovery")||"resource_discovery"<-InventoryList],
    [{ok,LogPaths}]=[catalog:get_paths("log")||"log"<-InventoryList],
    [{ok,AdderPaths}]=[catalog:get_paths("adder")||"adder"<-InventoryList],
    [{ok,DiviPaths}]=[catalog:get_paths("divi")||"divi"<-InventoryList],

    PaArgs1=lists:sort(lists:append([RdPaths,LogPaths,AdderPaths])),
    ["catalog/adder/ebin","catalog/log/ebin","catalog/resource_discovery/ebin"]=PaArgs1,

    [true,true,true]=[rpc:call(Node1,code,add_patha,[Path],5000)||Path<-PaArgs1],
   
    ok=rpc:call(Node1,application,start,[log],5000),
    pong=rpc:call(Node1,log,ping,[],5000),
    ok=rpc:call(Node1,application,start,[rd],5000),
    pong=rpc:call(Node1,rd,ping,[],5000),
    ok=rpc:call(Node1,application,start,[adder],5000),
    pong=rpc:call(Node1,adder,ping,[],5000),
    42=rpc:call(Node1,adder,add,[20,22],5000),
     
    %% staqrt divi on node 2  
    PaArgs2=lists:sort(lists:append([RdPaths,LogPaths,DiviPaths])),
    ["catalog/divi/ebin","catalog/log/ebin","catalog/resource_discovery/ebin"]=PaArgs2,
    [true,true,true]=[rpc:call(Node2,code,add_patha,[Path],5000)||Path<-PaArgs2],
   
    ok=rpc:call(Node2,application,start,[log],5000),
    pong=rpc:call(Node2,log,ping,[],5000),
    ok=rpc:call(Node2,application,start,[rd],5000),
    pong=rpc:call(Node2,rd,ping,[],5000),
    ok=rpc:call(Node2,application,start,[divi],5000),
    pong=rpc:call(Node2,divi,ping,[],5000),
    42.0=rpc:call(Node2,divi,divi,[420,10],5000),

    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_appl()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,InventoryList}=catalog:get_inventory(),
    %%-- The Applications are not cloned
    ErrorNotCloned=lists:sort([{ApplId,catalog:is_appl_updated(ApplId)}||ApplId<-InventoryList]),
    [
     {"adder",{error,{error,["Inventory doesnt exists, need to clone"]}}},
     {"divi",{error,{error,["Inventory doesnt exists, need to clone"]}}},
     {"log",{error,{error,["Inventory doesnt exists, need to clone"]}}},
     {"resource_discovery",{error,{error,["Inventory doesnt exists, need to clone"]}}}
    ]=ErrorNotCloned,

    %% Try to get paths not cloned
    [{"adder",_},
     {"divi",_},
     {"log",_},
     {"resource_discovery",_}
    ]=lists:sort([{ApplId,catalog:get_paths(ApplId)}||ApplId<-InventoryList]),
    
    %%-- Clone all applications
    Cloned=lists:sort([{ApplId,catalog:clone_appl(ApplId)}||ApplId<-InventoryList]),
    [{"adder",ok},{"divi",ok},{"log",ok},{"resource_discovery",ok}]=Cloned,
    [true,true,true,true]=[catalog:is_appl_updated(ApplId)||ApplId<-InventoryList],
    
    %%--- get paths 
   [
    {ok,["catalog/adder/ebin"]},
    {ok,["catalog/divi/ebin"]},
    {ok,["catalog/log/ebin"]},
    {ok,["catalog/resource_discovery/ebin"]}
   ]=[catalog:get_paths(ApplId)||ApplId<-InventoryList],

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
git_inventory()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,{error,["Inventory doesnt exists, need to clone"]}}=catalog:is_inventory_updated(),
    ok=catalog:clone_inventory(),
    true=catalog:is_inventory_updated(),

    {ok,InventoryList}=catalog:get_inventory(),
     [
      "adder","divi","log","resource_discovery"
     ]=lists:sort(InventoryList),

    {error,{error,["Already updated ","inventory"]}}=catalog:update_inventory(),

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    
    ok.
