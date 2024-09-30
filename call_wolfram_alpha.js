function call_Wolfram_alpha(params, userSettings) {
    const url = userSettings.wolframAlphaServerURL + 
        "/api/v1/wolframAlphaJSON?" + 
        new URLSearchParams({'query': params.query}); 
    
    var result = fetch(url)
        .then(response => { return response.json(); })
        .catch(err => console.error(err)); 
    
    return result;
}
