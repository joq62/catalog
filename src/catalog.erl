%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(catalog). 

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("catalog.hrl").



%% API

-export([
	 get_inventory/0,
	 is_inventory_updated/0,
	 update_inventory/0,
	 clone_inventory/0
	]).

-export([
	 get_paths/1,
	 is_appl_updated/1,
	 update_appl/1,
	 clone_appl/1
	 
	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		catalog_dir,
		inventory_dir,
		inventory_file,
		inventory_git
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%********************* Appl *****************************************


%%--------------------------------------------------------------------
%% @doc
%% get the full path to ebin and if presence the priv dirs to application
%% ApplId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_paths(ApplId :: string()) -> 
	  {ok,ListOfEbinAndPriv :: term()} | {error, Reason :: term()}.
get_paths(ApplId) ->
    gen_server:call(?SERVER,{get_paths,ApplId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_appl_updated(ApplId :: string()) -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_appl_updated(ApplId) ->
    gen_server:call(?SERVER,{is_appl_updated,ApplId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_appl(ApplId :: string()) -> 
	  ok | {error, Reason :: term()}.
update_appl(ApplId) ->
    gen_server:call(?SERVER,{update_appl,ApplId},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_appl(ApplId :: string()) -> 
	  ok | {error, Reason :: term()}.
clone_appl(ApplId) ->
    gen_server:call(?SERVER,{clone_appl,ApplId},infinity).





%%********************* Inventory ************************************

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_inventory() -> 
	  {ok,ListOfApplications :: term()} | {error, Reason :: term()}.
get_inventory() ->
    gen_server:call(?SERVER,{get_inventory},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_inventory_updated() -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_inventory_updated() ->
    gen_server:call(?SERVER,{is_inventory_updated},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_inventory() -> 
	  ok | {error, Reason :: term()}.
update_inventory() ->
    gen_server:call(?SERVER,{update_inventory},infinity).

%%--------------------------------------------------------------------
%% @doc
%% get_inventory   
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_inventory() -> 
	  ok | {error, Reason :: term()}.
clone_inventory() ->
    gen_server:call(?SERVER,{clone_inventory},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Return all repos   
%% 
%% @end
%%--------------------------------------------------------------------
-spec repos() -> 
	  {ok,ListOfRepos :: term()} | {error, Reason :: term()}.
repos() ->
    gen_server:call(?SERVER,{repos},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Returns the path to "ebin" dir and if needed "priv" dir 
%% related to Application Id  
%% 
%% @end
%%--------------------------------------------------------------------
-spec paths(ApplicationId :: string()) -> 
	  {ok,ListOfPaths :: term()} | {error, Reason :: term()}.
paths(ApplicationId) ->
    gen_server:call(?SERVER,{paths,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Resturn the app for application ApplicationId  
%% 
%% @end
%%--------------------------------------------------------------------
-spec app(ApplicationId::string()) -> 
	  {ok,App::atom()} | {error, Reason :: term()}.
app(ApplicationId) ->
    gen_server:call(?SERVER,{app,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% git merge for the repo LocalRepo 
%% 
%% @end
%%--------------------------------------------------------------------
-spec merge(LocalRepo::string()) -> ok | 
	  {error, Reason :: term()}.
merge(LocalRepo) ->
    gen_server:call(?SERVER,{merge,LocalRepo},infinity).

%%--------------------------------------------------------------------
%% @doc
%% stops all kubeletes and deletes their dirs 
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_cluster(ClusterId::string()) -> ok | 
	  {error, Error :: term()}.
delete_cluster(ClusterId) ->
    gen_server:call(?SERVER,{delete_cluster,ClusterId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
   
    file:make_dir(?CatalogDir),
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    catalog_dir=?CatalogDir,
	    inventory_dir=?InventoryDir,
	    inventory_file=?InventoryFile,
	    inventory_git=?InventoryGit
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%%********************* Appl *****************************************
handle_call({get_paths,ApplId}, _From, State) ->
    ApplDir=filename:join([State#state.catalog_dir,ApplId]),
    Result=try lib_catalog:get_paths(ApplDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Paths}->
		 % io:format("InventoryList ~p~n",[{InventoryList,?MODULE,?LINE}]),
		  NewState=State,
		  {ok,Paths};
	      ErrorEvent->
		%  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({is_appl_updated,ApplId}, _From, State) ->
    ApplDir=filename:join([State#state.catalog_dir,ApplId]),
    Result=try lib_catalog:is_inventory_updated(ApplDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		%  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({update_appl,ApplId}, _From, State) ->
    ApplDir=filename:join([State#state.catalog_dir,ApplId]),
    Result=try lib_catalog:update_inventory(ApplDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({clone_appl,ApplId}, _From, State) ->
    ApplDir=filename:join([State#state.catalog_dir,ApplId]),
    InventoryFile=State#state.inventory_file,
    Result=try lib_catalog:clone_appl(ApplId,ApplDir,InventoryFile) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

%%********************* Inventory ************************************
    
handle_call({get_inventory}, _From, State) ->
    InventoryFile=State#state.inventory_file,
    Result=try lib_catalog:get_inventory(InventoryFile) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,InventoryList}->
		 % io:format("InventoryList ~p~n",[{InventoryList,?MODULE,?LINE}]),
		  NewState=State,
		  {ok,InventoryList};
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({is_inventory_updated}, _From, State) ->
    InventoryDir=State#state.inventory_dir,
    Result=try lib_catalog:is_inventory_updated(InventoryDir) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		  NewState=State,
		  IsUpdated;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({update_inventory}, _From, State) ->
    InventoryDir=State#state.inventory_dir,
    Result=try lib_catalog:update_inventory(InventoryDir) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  %io:format("UpdateResult ~p~n",[{UpdateResult,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};

handle_call({clone_inventory}, _From, State) ->
    InventoryDir=State#state.inventory_dir,
    InventoryGit=State#state.inventory_git,
    Result=try lib_catalog:clone_inventory(InventoryDir,InventoryGit) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		%  io:format("CloneResult ~p~n",[{ok,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};
 

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    io:format("timeout ~p~n",[{?MODULE,?LINE}]),
   
    
    {noreply, State};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
