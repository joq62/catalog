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

 
%% API
-export([
	 
	 init/3
	
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
init(RepoDir,GitPath,ApplicationDir)->
    case rd:call(git_handler,is_repo_updated,[RepoDir],5000) of
	{error,["RepoDir doesnt exists, need to clone"]}->
	    ok=rd:call(git_handler,clone,[RepoDir,GitPath],5000);
	false ->
	    ok=rd:call(git_handler,update_repo,[RepoDir],5000);
	true ->
	    ok
    end,
    case filelib:is_dir(ApplicationDir) of
	false->
	    ok=file:make_dir(ApplicationDir);
	true->
	    ok
    end,
    {ok,AllFileNames}=rd:call(git_handler,all_filenames,[RepoDir],5000),
    file:del_dir_r(ApplicationDir),
    ok=file:make_dir(ApplicationDir),
    []=clone(AllFileNames,RepoDir,ApplicationDir),
    ok.

	       


%%%===================================================================
%%% Internal functions
%%%===================================================================
clone(FileNames,CatalogRepoDir,ApplicationDir)->
    clone(FileNames,CatalogRepoDir,ApplicationDir,[]).

clone([],_CatalogRepoDir,_ApplicationDir,Acc)->
    Acc;
clone([FileName|T],CatalogRepoDir,ApplicationDir,Acc)->
    NewAcc=case rd:call(git_handler,read_file,[CatalogRepoDir,FileName],5000) of
	       {ok,[Info]}->
		   %io:format("Info,FileName ~p~n",[{Info,FileName,?MODULE,?LINE}]),
		   RepoDir=maps:get(application_name,Info),
		   GitPath=maps:get(git,Info),
		   FullRepoDir=filename:join([ApplicationDir,RepoDir]),
		   ok=rd:call(git_handler,clone,[FullRepoDir,GitPath],5000),
		   Acc;
	      Error->
		  [{error,Error}|Acc]
	  end,
    clone(T,CatalogRepoDir,ApplicationDir,NewAcc).
