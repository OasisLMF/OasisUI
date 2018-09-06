shinyjs.changeJSGridTextVisibility = function(params) {
  var defaultParams = {
    index: null,
    visible: false
  };
  params = shinyjs.getParams(params, defaultParams);
  jsGridText = document.getElementsByClassName("js-grid-text-" + params.index)[0];
  if (jsGridText) {
    jsGridText.style.visibility = params.visible ? "visible" : "hidden";
  }
};

shinyjs.disableCheckboxes = function(params) {
  var defaultParams = {
    checkboxGroupInputId: null,
    disableIdx: []
  };
  params = shinyjs.getParams(params, defaultParams);
  var nodeList = document.getElementsByName(params.checkboxGroupInputId);
  if (nodeList.length > 0) {
    // enable all checkboxes
    var i;
    for (i = 0; i < nodeList.length; i++) {
      nodeList[i].disabled = false;
    }
    // disable those in disableIdx
    for (i = 0; i < params.disableIdx.length; i++) {
      nodeList[params.disableIdx[i]].disabled = true;
    }
  }
};