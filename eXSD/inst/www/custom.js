//let open_tab = (new URL(window.location).searchParams.get('method') || []);
//console.warn("open_tab", open_tab);

window.addEventListener('hashchange', function(e) {
    let hash = new URL(e.newURL).hash;
    if(hash && hash.startsWith("#shiny-tab")){
        let tabname = hash.split("-", 3)[2];
        document.querySelector(`a[data-value='${tabname}']`).click()
        console.warn("RSHINY TWEAK: Go to", tabname);
    }
})

const forceChangeHash = (e) => {
    history.replaceState(null, null, ' ');
    console.warn("RSHINY TWEAK: Removing hash");
}

document.addEventListener("DOMContentLoaded", () => {
    Array.from(document.querySelectorAll("*[data-toggle][href*='shiny-tab']")).forEach((e) => e.addEventListener("click", forceChangeHash));
});

/*document.addEventListener("DOMContentLoaded", () => {
    tab_names = Array.from(document.querySelectorAll("*[data-toggle][href*='shiny-tab']")).map((e) => e.hash.split("-", 3)[2]);
    console.log(tab_names);
});
*/

/*
window.onhashchange = function () {
        hashChanged(window.location.hash);
    }
*/