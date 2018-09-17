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
    // single indices are not passed by R as an array
    if (!(params.disableIdx instanceof Array)) {
      params.disableIdx = [params.disableIdx];
    }
    // disable checkboxes according to disableIdx
    for (i = 0; i < params.disableIdx.length; i++) {
      nodeList[params.disableIdx[i]].disabled = true;
    }
  }
};

shinyjs.updateStepColors = function(params) {
  var defaultParams = {
    radioButtonsId: null
  };
  params = shinyjs.getParams(params, defaultParams);
  var nodeList = document.querySelectorAll('#' + params.radioButtonsId + ' button');
  if (nodeList.length > 0) {
    var i, selectedButtonIdx = nodeList.length - 1;
    for (i = 0; i < nodeList.length; i++) {
      nodeList[i].classList.remove("previousStepButton");
      nodeList[i].classList.remove("activeStepButton");
      nodeList[i].classList.remove("nextStepButton");
      var classes = nodeList[i].className.split(" ");
      // check whether the current button is the active one
      if ($.inArray("active", classes) > -1) {
        selectedButtonIdx = i;
        nodeList[i].classList.add("activeStepButton");
      }
      if (i < selectedButtonIdx) {
        nodeList[i].classList.add("previousStepButton");
      }
      if (i > selectedButtonIdx) {
        nodeList[i].classList.add("nextStepButton");
      }
    }
  }
};

shinyjs.reset = function() {history.go(0)}