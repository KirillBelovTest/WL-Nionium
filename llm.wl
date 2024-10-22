(* ::Package:: *)

(* ::Section::Closed:: *)
(*Init*)


<<KirillBelov`AILink`


<<KirillBelov`RocketChatLink`


settings = <|
	"Endpoint" -> "https://chat.nionium.ai", 
	"Name" -> "llm.bot", 
	"UserId" -> "eh5zpdNCGWhcoiTQf", 
	"AuthToken" -> "rq9eZTIpxhX7m0JdlBzbtdxuhQvlyg0cXOoI3WN0z83", 
	"WebSocketEndpoint" -> "wss://chat.nionium.ai/websocket"
|>; 


(* ::Section::Closed:: *)
(*Rocket Chat*)


id := IntegerString[RandomInteger[1500625], 36, 4]; 


rid = "eh5zpdNCGWhcoiTQfnCEnDHPBgACezER2z"; 


systemPrompt = "If you are working with tabular data, 
you should format all lists and tables as markdown tables. 
And then frame them as a multi-line code block in markdown with (```). "; 


(* ::Subsubsection:: *)
(*all chats id*)


getChats[settings_] := With[{s = settings}, 
	Join[
		Map[{#, "d"}&] @ 
		Query["update", Select[#["t"] === "d"&], "_id"] @ 
		getRooms[s], 
		
		Map[{#, "g"}&] @ 
		Query["groups", All, "_id"] @ 
		getGroups[s]
	]
]; 


(* ::Subsubsection:: *)
(*rooms list*)


getRooms[settings_, time_] := (
	If[!AssociationQ[$rooms], 
		$rooms = <||>
	];
	If[!KeyExistsQ[$rooms, time], 
		$rooms[time] = RocketChatRoomsGet[settings]
	];
	$rooms[time]
); 


getRooms[settings_] := getRooms[settings, Round[AbsoluteTime[], 60]]; 


(* ::Subsubsection:: *)
(*groups list*)


getGroups[settings_, time_] := (
	If[!AssociationQ[$groups], 
		$groups = <||>
	];
	If[!KeyExistsQ[$groups, time], 
		$groups[time] = RocketChatGroupsList[settings]
	];
	$groups[time]
); 


getGroups[settings_] := getGroups[settings, Round[AbsoluteTime[], 60]]; 


(* ::Section:: *)
(*Update handler*)


lastUpdate[rid_] := Module[{t}, 
	If[!AssociationQ[$lastUpdate], $lastUpdate = <||>]; 
	t = If[KeyExistsQ[$lastUpdate, rid], $lastUpdate[rid], Now]; 
	$lastUpdate[rid] = Now; 
	t
];


convertToReadable[text_String] /; StringContainsQ[text, "\[CapitalEth]"] := 
If[StringContainsQ[#, "\[CapitalEth]"], text, #]& @ 
FromCharacterCode[ToCharacterCode[text, "ISO8859-1"], "UTF8"]; 


convertToReadable[text_String] := text; 


convertToReadable[expr_] := ToString[expr]; 


handleUpdates[settings_, handler_] := 
Module[{
	updates
}, 
	updates = 
		Association @ 
		KeyValueMap[Function[{k, v}, k[[1]] -> Map[Function[u, Append[u, "t" -> k[[2]]]], v]], #]& @ 
		Query[All, "result", "updated"] @ 
		Association @ 
		Map[# -> RocketChatSyncMessages[settings, #[[1]], "LastUpdate" -> lastUpdate[#[[1]]]]&] @ 
		getChats[settings];
	
	Map[handleUpdates[settings, handler, #]&] @ updates; 
]; 


handleUpdates[settings_, handler_, updates_List] := 
(Map[handleUpdates[settings, handler, #]&, updates]; );


handleUpdates[settings_, handler_, update_Association] := 
Module[{result}, 
	If[Head[$handled] =!= DataStructure || $handled["Length"] > 1024, 
		$handled = CreateDataStructure["HashSet"]
	]; 
	
	If[
		Not[$handled["MemberQ", update["_id"]]] && 
		update["u", "username"] =!= "llm.bot" && 
		(update["t"] === "d" || StringContainsQ[update["msg"], "@llm.bot"]), 
		
		$handled["Insert", update["_id"]]; 
		result = handler[update]; 
		If[StringQ[result], 
			RocketChatSendMessage[settings, update["rid"], result]
		]
	]; 
]; 


(* ::Section::Closed:: *)
(*Wolfram Alpha tool-function*)


wolframAlpha::usage = "wolframAlpha[query] request to Wolfram Alpha.
### FORMATTING RULES:

If Wolfram Alpha returns lists or tables - format result as MD table. 
Remove original link to Wolfram Alpha.

### Concise Description of Wolfram Alpha Functionality:

**Function Description:**
Wolfram Alpha is a computational engine that provides answers and performs calculations across various domains such as mathematics, science, history, finance, and more. It processes queries in English and returns accurate results by utilizing structured data and algorithms.

**Key Features:**
- Solves mathematical equations, calculus.
- Provides scientific data,  historical facts.
- Offers financial information.

**Additional Points:**
- Queries should be simple.
- Responses in English.

Wolfram Alpha is a valuable tool for obtaining precise and useful information for both educational and practical applications."; 


wolframAlpha[query_String] := URLRead[Internal`HouseKeep[
	"https://api.wolframalpha.com/v1/llm-api", 
	{"input" -> URLEncode[query]}
]]["Body"]; 


(* ::Section:: *)
(*DuckDuckGo search*)


duckDuckGoSearch::usage = "duckDuckGoSearch[query] call DuckDuckGo search engine.  
### Concise Description: duckDuckGoSearch

- query must be only in English.

**Purpose:**
- Perform internet searches via DuckDuckGo's API, returning results in JSON format.

**Key Features:**
- Retrieves concise search results by minimizing redirects and unnecessary content.
- Suitable for quick information retrieval without distractions like ads.

**Usage:**
- Facilitates streamlined search operations for precise and actionable data in LLM environments."; 


duckDuckGoSearch[query_String] := 
Module[{
	url = "https://lite.duckduckgo.com/lite/", 
	request, 
	response, responseBody
}, 
	request = HTTPRequest[url, 
		<|
			Method -> "POST", 
			"Body" -> {
				"q" -> query
			}, 
			"ContentType" -> "application/x-www-form-urlencoded"
		|>
	]; 
	
	response = URLRead[request]; 
	
	responseBody = response["Body"];
	
	ImportString[ExportString[responseBody, "String"], "HTML"] <> "\n" <> 
	"https://duckduckgo.com?" <> URLQueryEncode[{"q" -> query}]
]; 


(* ::Section:: *)
(*urlRead*)


urlRead::usage = 
"urlRead[url] read text from html page with address == url.
Use this function when you need to get data from the specific address or site in the internet. 
result is a imple text."; 


urlRead[url_String] := 
With[{$url = url}, 
	Module[{response}, 
		response = CloudEvaluate[URLRead[url]]; 
		ImportString[response["Body"], "HTML"]
	]
]; 



(* ::Section::Closed:: *)
(*Wikipedia*)


wiki::usage = 
"wiki[keyword] search information about the keyword in Wikipedia. 

keyword - single keyword or phrase.

### Concise Description: WikipediaSearch

**Purpose:**
- Perform keyword-based searches within Wikipedia's vast repository of articles to retrieve relevant information.

**Key Features:**
- Retrieves concise summaries or detailed information from Wikipedia articles based on the specified keywords.
- Provides links to the full articles for further reading and exploration.
- Supports a wide range of topics, including history, science, technology, culture, and more.

**Usage:**
- Facilitates quick access to verified and comprehensive information from Wikipedia, suitable for educational and research purposes.

**Parameters:**
- `query`: A string parameter that specifies the keywords or phrase to search within Wikipedia.

**Response:**
- Returns a summary or excerpt from the Wikipedia article(s) related to the search query, along with a link to the full article."; 


wiki[keyword_String] := 
StringRiffle[Map[WikipediaData] @ WikipediaSearch["Content" -> keyword][[ ;; UpTo[5]]], "\n\n"]; 


(* ::Sction:: *)
(*ClearChat*)


clearChat::usage = 
"clearChat[] remove chat history from the current chat."; 


clearChat[] := (
	$chats[$rid] = AIChatObject[]; 
	"Chat history removed. Now the bot can starts conversation from scratch."
)


(* ::Sction:: *)
(*ClearChat*)


dialogSize::usage = 
"dialogSize[] returns count of messages of the current chat."; 


dialogSize[] := 
ToString[Length[$chats[$rid]]]; 



(* ::Section:: *)
(*Currency rates*)


currencyRates[] := 
URLRead["https://api.freecurrencyapi.com/v1/latest?apikey=fca_live_J84s8Ts3gqdvYxNL705JyfPAZtkSc22dbJw9SLJv"]["Body"]; 


currencyRates::usage = 
"currencyRates[] provides current currency rates in JSON format.";


(* ::Section::Closed:: *)
(*GPT completion*)


If[!AssociationQ[$chats], $chats = <||>]; 


gpt[update_Association] := 
Function[
	If[!KeyExistsQ[$chats, #rid],  
		$chats[#rid] = AIChatObject[]; 
		With[{c = $chats[#rid]}, 
			c["Messages"] = {<|"role" -> "system", "content" -> systemPrompt|>}
		]
	]; 
	With[{chat = $chats[#rid]},  
		$rid = #rid; 
		chat["Messages"] = MapAt[convertToReadable, chat["Messages"], {All, "content"}]; 
		
		AIChatComplete[chat, convertToReadable[#msg], 
			"Model" -> "gpt-4o", 
			"Tools" -> $tools, 
			"Logger" -> Function[Echo[#Body["Body"], "ChatGPT Answer:"]], 
			"Evaluator" -> CloudEvaluate
		]; 
		chat[-1]["content"]
	]
][update]; 


(* ::Section:: *)
(*Tools*)


$tools = {
	wolframAlpha, 
	duckDuckGoSearch, 
	urlRead, 
	wiki, 
	currencyRates, 
	clearChat, 
	dialogSize
}; 


(* ::Section:: *)
(*Entrypoint*)


Function[handleUpdates[settings, 
	Function[
		Echo[ImportString[#msg, "Text"], "MESSAGE:"]; 
		Echo[convertToReadable[gpt[#]], "ANSWER:"]
	]
]]
