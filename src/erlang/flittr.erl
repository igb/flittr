-module(flittr).

-export([get_photos/2,search_user_photos/4,search_photos/4,query/1,photo_source_url_from_photoref/1, web_page_url_from_photoref/1,get_person/3,owner_from_photoref/1,get_license/1,id_from_photoref/1,get_photo_info/2,get_photoset/3]).

get_photos(UserId, ApiKey)->
  Url=lists:flatten(["http://api.flickr.com/services/rest/?method=flickr.people.getPublicPhotos&api_key=", ApiKey, "&user_id=", UserId, "&format=rest"]), 
  query(Url).	

get_photoset(UserId, PhotoSetId, ApiKey)->
  Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photosets.getPhotos&api_key=", ApiKey, "&user_id=", UserId, "&photoset_id=", PhotoSetId, "&format=rest"]), 
  query(Url).	

get_person(UserId, ApiKey, PersonId)->
    Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.people.getInfo&api_key=", ApiKey, "&user_id=", PersonId, "&format=rest"]),
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {Url, []}, [], []),
      Response=parse_xml(Body),
      {rsp,[{stat,"ok"}],RespBody}=Response,
      [_|[Person|_]]=RespBody,
      {person,
         [{id,Id},
          {nsid,NsId},
          {ispro,IsPro},
          {can_buy_pro,CanBuyPro},
          {iconserver,IconServer},
          {iconfarm,IconFarm},
          {path_alias,PathAlias},
          {has_stats,HasStats}],RawData}=Person,
           RawXml=lists:filter(fun(X)-> case X of "\n\t" -> false; "\n" -> false; _ -> true end end, RawData),
       MetaData=lists:map(fun(X)-> {Key,_,Data}=X, {Key,lists:flatten(Data)} end,RawXml),
       [{id,Id},
          {nsid,NsId},
          {ispro,IsPro},
          {can_buy_pro,CanBuyPro},
          {iconserver,IconServer},
          {iconfarm,IconFarm},
          {path_alias,PathAlias},
          {has_stats,HasStats},
	  {metadata,MetaData}].
      


query(Url)->
{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {Url, []}, [], []),
      Response=parse_xml(Body),
      {rsp,[{stat,"ok"}],RespBody}=Response,
      [_|[Photos|_]]=RespBody,
      case Photos of
      	   {photos,_,_} ->RefParser=fun(Ref)->[{id,Id},
				      {owner,Owner},
   				      {secret,Secret},
   				      {server,Server},
   				      {farm,Farm},
   				      {title,Title},
   				      {ispublic,IsPublic},
   				      {isfriend,IsFriend},
   				      {isfamily,IsFamily}]=Ref,
				      {Id, Owner, Secret,Server,Farm,Title,IsPublic, IsFriend, IsFamily} 
				      end,
				      {photos,[{page, Page},{pages, Pages},{perpage, PerPage}, {total, Total}],PhotoRefs}=Photos;
	   {photoset,_,_} -> RefParser=fun(Ref)->[{id,Id},
   				      {secret,Secret},
   				      {server,Server},
   				      {farm,Farm},
   				      {title,Title},
				      {isprimary,IsPrimary},
   				      {ispublic,IsPublic},
   				      {isfriend,IsFriend},
   				      {isfamily,IsFamily}]=Ref,
				      {Id,Secret,Server,Farm,Title,IsPublic, IsPrimary, IsFriend, IsFamily}
      				      end,
				      {photoset,[{id, Id},{primary, Primary},{owner, Owner},{ownername, OwnerName}, {page, Page},{per_page, PerPage},{perpage, PerPage}, {pages, Pages},{total, Total}, {title, Title}],PhotoRefs}=Photos
	end,   
      
      
      Refs=lists:map(fun({_,Ref,_})->RefParser(Ref)
				     end,
				       lists:filter(fun(X)-> 
				       			     case X of 
							     	  "\n\t" -> false; 
							     	  "\n" -> false; 
								  _ -> true 
							     end 
						  end, PhotoRefs)
			),
      {Page, Pages, PerPage, Total, Refs}.

search_photos(UserId, ApiKey, SearchTerm, License)->
     Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", ApiKey,  "&text=", http_uri:encode(SearchTerm), "&license=", License, "&format=rest"]),
     query(Url).

search_user_photos(UserId, ApiKey, SearchTerm, License)->
     Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=", ApiKey, "&user_id=", UserId,"&text=", http_uri:encode(SearchTerm), "&license=", License, "&format=rest"]),
     query(Url).

get_photo_info(PhotoId,ApiKey)->
	Url=lists:flatten(["https://api.flickr.com/services/rest/?method=flickr.photos.getInfo&api_key=", ApiKey,  "&photo_id=", PhotoId, "&format=rest"]),
	    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} =
      httpc:request(get, {Url, []}, [], []),
      Response=parse_xml(Body),
      {rsp,[{stat,"ok"}],RespBody}=Response,
      [_,Photo,_]=RespBody,
      {photo, Attributes,_}=Photo,
      Attributes. 

photo_source_url_from_photoref({Id, Owner, Secret,Server,Farm,Title,IsPublic, IsFriend, IsFamily}) ->
  lists:flatten(["https://farm", Farm, ".staticflickr.com/",Server,"/",Id,"_",Secret,"_b.jpg"]).

owner_from_photoref({_,Owner, _,_,_,_,_, _, _})->
  Owner.

id_from_photoref({Id,_, _,_,_,_,_, _, _}) ->
  Id.


web_page_url_from_photoref({Id,Owner, _,_,_,_,_, _, _}) ->
  lists:flatten(["https://www.flickr.com/photos/", Owner, "/", Id]).

parse_xml(Message)->
    {Xml, _}=xmerl_scan:string(Message),
    xmerl_lib:simplify_element(Xml).

get_license(Id)->
  case Id of
   "0" -> {"All Rights Reserved", ""};
   "1" -> {"Attribution-NonCommercial-ShareAlike License", "http://creativecommons.org/licenses/by-nc-sa/2.0/"};
   "2" -> {"Attribution-NonCommercial License", "http://creativecommons.org/licenses/by-nc/2.0/"};
   "3" -> {"Attribution-NonCommercial-NoDerivs License", "http://creativecommons.org/licenses/by-nc-nd/2.0/"};
   "4" -> {"Attribution License", "http://creativecommons.org/licenses/by/2.0/"};
   "5" -> {"Attribution-ShareAlike License", "http://creativecommons.org/licenses/by-sa/2.0/"};
   "6" -> {"Attribution-NoDerivs License", "http://creativecommons.org/licenses/by-nd/2.0/"};
   "7" -> {"No known copyright restrictions", "http://flickr.com/commons/usage/"};
   "8" -> {"United States Government Work", "http://www.usa.gov/copyright.shtml"}
 end.