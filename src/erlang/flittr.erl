-module(flittr).

-export([get_photos/2,search_photos/4,query/1,photo_source_url_from_photoref/1, web_page_url_from_photoref/1]).

get_photos(UserId, ApiKey)->
  Url=lists:flatten(["http://api.flickr.com/services/rest/?method=flickr.people.getPublicPhotos&api_key=", ApiKey, "&user_id=", UserId, "&format=rest"]), 
  query(Url).	

query(Url)->
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {Url, []}, [], []),
      Response=parse_xml(Body),
      {rsp,[{stat,"ok"}],RespBody}=Response,
      [_|[Photos|_]]=RespBody,
      {photos,[{page, Page},{pages, Pages},{perpage, PerPage}, {total, Total}],PhotoRefs}=Photos,
      Refs=lists:map(fun({_,Ref,_})->[{id,Id},
   {owner,Owner},
   {secret,Secret},
   {server,Server},
   {farm,Farm},
   {title,Title},
   {ispublic,IsPublic},
   {isfriend,IsFriend},
   {isfamily,IsFamily}]=Ref,{Id, Owner, Secret,Server,Farm,Title,IsPublic, IsFriend, IsFamily} end, lists:filter(fun(X)-> case X of "\n\t" -> false; "\n" -> false; _ -> true end end, PhotoRefs)),
      {Page, Pages, PerPage, Total, Refs}.

search_photos(UserId, ApiKey, SearchTerm, License)->
     Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", ApiKey,  "&text=", http_uri:encode(SearchTerm), "&license=", License, "&format=rest"]),
     query(Url).

photo_source_url_from_photoref({Id, Owner, Secret,Server,Farm,Title,IsPublic, IsFriend, IsFamily}) ->
  lists:flatten(["https://farm", Farm, ".staticflickr.com/",Server,"/",Id,"_",Secret,"_b.jpg"]).

web_page_url_from_photoref({Id, Owner, Secret,Server,Farm,Title,IsPublic, IsFriend, IsFamily}) ->
  lists:flatten(["https://www.flickr.com/photos/", Owner, "/", Id]).

parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).