%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_catalog).
  

-define(UpToDate,"Up to date").
-define(NotUpToDate,"Not up to date").
 
%% API
-export([

	]).

-export([
	 get_inventory/1,
	 get_tags/1,
	 is_inventory_updated/1,
	 update_inventory/1,
	 clone_inventory/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_inventory(InventoryFile)->
    {ok,Info}=file:consult(InventoryFile),
    ApplicationIdList=[maps:get(id,Map)||Map<-Info],
    {ok,ApplicationIdList}.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_inventory_updated(InventoryDir)->
    Result=case filelib:is_dir(InventoryDir) of
	       false->
		   {error,["Inventory doesnt exists, need to clone"]};
	       true->
		   {ok,is_up_to_date(InventoryDir)}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_inventory(InventoryDir)->
    true=filelib:is_dir(InventoryDir),
    Result=merge(InventoryDir),   
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone_inventory(InventoryDir,InventoryGit)->
    file:del_dir_r(InventoryDir),
    ok=file:make_dir(InventoryDir),
    Result=clone(InventoryDir,InventoryGit),   
    Result.



%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

get_tags(LocalRepo)->
    TagString=os:cmd("git -C "++LocalRepo++" "++"tag"),
    T1=[S||S<-string:split(TagString, "\n", all),
	     []=/=S],
    Tags=lists:reverse(lists:sort(T1)),
    Tags.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
merge(LocalRepo)->
    Result=case is_up_to_date(LocalRepo) of
	       false->
		   os:cmd("git -C "++LocalRepo++" "++"merge  "),
		   ok;
	       true->
		   {error,["Already updated ",LocalRepo]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

clone(InventoryDir,InventoryGit)->
    []=os:cmd("git clone -q "++InventoryGit++" "++InventoryDir),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_up_to_date(LocalRepo)->

    _Fetch=os:cmd("git -C "++LocalRepo++" "++"fetch origin "),
    Status=os:cmd("git -C "++LocalRepo++" status -uno | grep -q 'Your branch is up to date'  && echo Up to date || echo Not up to date"),
    [FilteredGitStatus]=[S||S<-string:split(Status, "\n", all),
			  []=/=S],
    Result=case FilteredGitStatus of
	       ?UpToDate->
		   true;
	       ?NotUpToDate->
		   false
	   end,
    Result.
