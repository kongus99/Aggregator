function activateElm(moduleName) {

    var app = Elm[moduleName].embed(document.getElementById('mainBody'), window.location.href);

    app.ports.elmAddressChange.subscribe(function(address) {
        window.history.replaceState({},"@title",address);
    });
}