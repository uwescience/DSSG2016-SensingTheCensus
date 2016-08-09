(window.onload = function() {
    
    setTimeout(function () {
    var leaf_widgets = Array.prototype.map.call(

      document.querySelectorAll(".leaflet"),
      function(ldiv){
        return HTMLWidgets.find("#" + ldiv.id);
      }
    );

    // make this easy since we know only two maps
    leaf_widgets[0].sync(leaf_widgets[1]);
    leaf_widgets[1].sync(leaf_widgets[0]);  
}, 40000);
})();