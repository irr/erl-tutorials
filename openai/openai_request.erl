-module(openai_request).

-export([start/0, post_request/0, stop/0]).

% Start the inets application
start() ->
    ssl:start(),
    inets:start().

% Function to make the POST request
post_request() ->
    % Retrieve the OpenAI API Key from the environment
    OpenAI_API_Key = os:getenv("OPENAI_API_KEY"),

    % Check if the API Key is retrieved successfully
    case OpenAI_API_Key of
        false -> 
            io:format("Environment variable OPENAI_API_KEY is not set.~n");
        _ ->
            % Define the URL
            Url = "https://api.openai.com/v1/chat/completions",

            % Define the headers
            Headers = [
                {"Content-Type", "application/json"},
                {"Authorization", "Bearer " ++ OpenAI_API_Key}
            ],

            % Define the body data
            Body = "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"user\", \"content\": \"Say this is a test!\"}], \"temperature\": 0.7}",

            % Make the POST request
            Response = httpc:request(post, {Url, Headers, "application/json", Body}, [{ssl, [{verify, verify_none}]}], []),

            % Check the response and print the JSON
            case Response of
                {ok, {{_, 200, _}, _, RespBody}} ->
                    io:format("Response: ~s~n", [RespBody]);
                {error, _} = Error ->
                    io:format("Error: ~p~n", [Error])
            end
    end.

% Function to stop the inets application
stop() ->
    ssl:stop(),
    inets:stop().


% curl https://api.openai.com/v1/chat/completions \
%  -H "Content-Type: application/json" \
%  -H "Authorization: Bearer $OPENAI_API_KEY" \
%  -d '{
%     "model": "gpt-4",
%     "messages": [{"role": "user", "content": "Say this is a test!"}],
%     "temperature": 0.7
%   }'
