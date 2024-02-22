%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_catalog).
  
-include("catalog.hrl").
-define(UpToDate,"Up to date").
-define(NotUpToDate,"Not up to date").
 
%% API
-export([
	 get_info/3,
	 check_update_repo_return_maps/2
	 
	]).

-export([
	 is_repo_updated/1,
	 update_repo/1,
	 clone_repo/2
	]).

%%%===================================================================
%%% API
%%%===================================================================





%%********************* Host *****************************************    
get_info(Key,ApplicationId,SpecMaps)->
    Result=case [Map||Map<-SpecMaps,
		      ApplicationId==maps:get(id,Map)] of
	       []->
		   {error,["ApplicationId doens't exists",ApplicationId]};
	       [Map]->
		   case maps:get(Key,Map) of
		       {badkey,Key}->
			   {error,["Badkey ",Key]};
		       Value->
			   {ok,Value}
		   end
	   end,
    Result. 


%%********************* Repo ************************************


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_update_repo_return_maps(RepoDir,RepoGit)->
    case is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone",RepoDir]}->
	    ok=clone_repo(RepoDir,RepoGit);
	{ok,false} ->
	    ok=update_repo(RepoDir);
	{ok,true}->
	    ok
    end,
    
    {ok,AllFileNames}=file:list_dir(RepoDir),
    AllFullFilenames=[filename:join([RepoDir,FileName])||FileName<-AllFileNames],
    HostFiles=[FullFileName||FullFileName<-AllFullFilenames,
			     ?Extension==filename:extension(FullFileName)],
    FileConsult=[file:consult(HostFile)||HostFile<-HostFiles],
    HostSpecMaps=[Map||{ok,[Map]}<-FileConsult],
    {ok,HostSpecMaps}. 
	       
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_repo_updated(RepoDir)->
    Result=case filelib:is_dir(RepoDir) of
	       false->
		   {error,["RepoDir doesnt exists, need to clone",RepoDir]};
	       true->
		   {ok,is_up_to_date(RepoDir)}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_repo(RepoDir)->
    true=filelib:is_dir(RepoDir),
    Result=merge(RepoDir),   
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone_repo(RepoDir,RepoGit)->
    file:del_dir_r(RepoDir),
    ok=file:make_dir(RepoDir),
    Result=clone(RepoDir,RepoGit),   
    Result.



%%%===================================================================
%%% Internal functions
%%%===================================================================

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

clone(RepoDir,RepoGit)->
    []=os:cmd("git clone -q "++RepoGit++" "++RepoDir),
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
