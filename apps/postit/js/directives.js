
function add_draggable(module) {
    // from https://docs.angularjs.org/guide/directive
    module.directive('postitDraggable', function($document) {
	return {
	    link: function(scope, element, attr) {
		var startX = 0, startY = 0, x = 0, y = 0;
		
		element.css({
		    position: 'relative',
		    cursor: 'pointer'
		});
		
		element.on('mousedown', function(event) {
		    // Prevent default dragging of selected content
		    event.preventDefault();
		    startX = event.pageX - x;
		    startY = event.pageY - y;
		    $document.on('mousemove', mousemove);
		    $document.on('mouseup', mouseup);
		});
		
		function mousemove(event) {
		    y = event.pageY - startY;
		    x = event.pageX - startX;
		    element.css({
			top: y + 'px',
			left:  x + 'px'
		    });
		}
		
		function mouseup() {
		    $document.off('mousemove', mousemove);
		    $document.off('mouseup', mouseup);
		}
	    }
	};
    });
}

function add_compare_to(module) {
    // from http://odetocode.com/blogs/scott/archive/2014/10/13/confirm-password-validation-in-angularjs.aspx
    module.directive('compareTo', function() {
	return {
            require: "ngModel",
            scope: {
		otherModelValue: "=compareTo"
            },
            link: function(scope, element, attributes, ngModel) {
		
		ngModel.$validators.compareTo = function(modelValue) {
                    return modelValue === scope.otherModelValue;
		};
		
		scope.$watch("otherModelValue", function() {
                    ngModel.$validate();
		});
            }
	};
    })
}
