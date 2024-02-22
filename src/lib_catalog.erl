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
	 get_map/2,
	 check_update_repo_return_maps/3
	 
	]).

-export([
	 is_repo_updated/1,
	 update_repo/1,
	 clone_repo/2
	]).

-export([
	 get_application_app/3,
	 get_application_paths/3,
	 is_application_repo_updated/3,
	 update_application_repo/3,
	 clone_application_repo/3
	]).

%%%===================================================================
%%% API
%%%===================================================================





%%********************* Host *****************************************    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_map(ApplicationId,SpecMaps)->
    Result=case [Map||Map<-SpecMaps,
		      ApplicationId==maps:get(id,Map)] of
	       []->
		   {error,["ApplicationId doens't exists",ApplicationId]};
	       [Map]->
		   {ok,Map}
	   end,
    Result. 
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
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
get_application_app(ApplicationDir,ApplicationId,SpecMaps)->
    get_info(app,ApplicationId,SpecMaps).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_application_paths(ApplicationDir,ApplicationId,SpecMaps)->
    {ok,ApplicationName}=get_info(application_name,ApplicationId,SpecMaps),
    ApplicationLocalRepo=filename:join([ApplicationDir,ApplicationName]),
    true=filelib:is_dir(ApplicationLocalRepo),
    Ebin=filename:join([ApplicationLocalRepo,"ebin"]),
    true=filelib:is_dir(Ebin),
    Priv=filename:join([ApplicationLocalRepo,"priv"]),
    Result=case filelib:is_dir(Priv) of
	       false->
		   {ok,[Ebin]};
	       true->
		    {ok,[Ebin,Priv]}
	   end,
    Result.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_application_repo_updated(ApplicationDir,ApplicationId,SpecMaps)->
    {ok,ApplicationName}=get_info(application_name,ApplicationId,SpecMaps),
    ApplicationLocalRepo=filename:join([ApplicationDir,ApplicationName]),
    Result= case filelib:is_dir(ApplicationLocalRepo) of
		false->
		    {error,["Applications local repo doesnt exists",ApplicationLocalRepo]};			    
		true->
			    {ok,is_up_to_date(ApplicationLocalRepo)}
	    end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
update_application_repo(ApplicationDir,ApplicationId,SpecMaps)->
    {ok,ApplicationName}=get_info(application_name,ApplicationId,SpecMaps),
    ApplicationLocalRepo=filename:join([ApplicationDir,ApplicationName]),
    true=filelib:is_dir(ApplicationLocalRepo),
    merge(ApplicationLocalRepo).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
clone_application_repo(ApplicationDir,ApplicationId,SpecMaps)->
    {ok,ApplicationName}=get_info(application_name,ApplicationId,SpecMaps),
    ApplicationLocalRepo=filename:join([ApplicationDir,ApplicationName]),
    file:del_dir_r(ApplicationLocalRepo),
    ok=file:make_dir(ApplicationLocalRepo),
    {ok,Git}=get_info(git,ApplicationId,SpecMaps),
    clone(ApplicationLocalRepo,Git).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
check_update_repo_return_maps(RepoDir,RepoGit,ApplicationDir)->
    case is_repo_updated(RepoDir) of
	{error,["RepoDir doesnt exists, need to clone",RepoDir]}->
	    ok=clone_repo(RepoDir,RepoGit);
	{ok,false} ->
	    ok=update_repo(RepoDir);
	{ok,true}->
	    ok
    end,
    case filelib:is_dir(ApplicationDir) of
	false->
	    ok=file:make_dir(ApplicationDir);
	true->
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
    Result=case os:cmd("git clone -q "++RepoGit++" "++RepoDir) of
	       []->
		   ok;
	       Reason->
		   {error,["Failed to clone ",RepoDir,RepoGit,Reason]}
	   end,
    Result.

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
