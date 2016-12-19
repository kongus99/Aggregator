function activateElm(moduleName) {
    var urlParams;
    (window.onpopstate = function () {
        var match,
            pl     = /\+/g,  // Regex for replacing addition symbol with a space
            search = /([^&=]+)=?([^&]*)/g,
            decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
            query  = window.location.search.substring(1);

        urlParams = {};
        while (match = search.exec(query))
            urlParams[decode(match[1])] = decode(match[2]);
    })();

    var app = Elm[moduleName].embed(document.getElementById('mainBody'), JSON.stringify(urlParams));

    app.ports.elmAddressChange.subscribe(function(address) {
        window.history.pushState({},"@title",address);
    });
}