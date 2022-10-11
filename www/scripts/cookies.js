function getCookies(){
  var res = Cookies.get();
  console.log(res);
  Shiny.setInputValue('cookies', res);
}

shinyjs.setCookie = function(params) {
  console.log(params);
  Cookies.set(params.id, params.value);
  getCookies();
};

shinyjs.getCookie = function(name) {
  Cookies.get(name);
  getCookies();
};

shinyjs.removeCookie = function(name) {
  Cookies.remove(name);
  getCookies();
};

$(document).on('shiny:connected', function(ev){
  getCookies();
});
