

angular.module('savageWorlds').controller('EditPowerController', function($scope, $routeParams, $location, PowerResource , TrappingResource) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.power = new PowerResource(self.original);
            TrappingResource.queryAll(function(items) {
                $scope.trappingsSelectionList = $.map(items, function(item) {
                    var wrappedObject = {
                        id : item.id
                    };
                    var labelObject = {
                        value : item.id,
                        text : item.name
                    };
                    if($scope.power.trappings){
                        $.each($scope.power.trappings, function(idx, element) {
                            if(item.id == element.id) {
                                $scope.trappingsSelection.push(labelObject);
                                $scope.power.trappings.push(wrappedObject);
                            }
                        });
                        self.original.trappings = $scope.power.trappings;
                    }
                    return labelObject;
                });
            });
        };
        var errorCallback = function() {
            $location.path("/Powers");
        };
        PowerResource.get({PowerId:$routeParams.PowerId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.power);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.power.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/Powers");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/Powers");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.power.$remove(successCallback, errorCallback);
    };
    
    $scope.rankList = [
        "Novice",  
        "Seasoned",  
        "Veteran",  
        "Heroic",  
        "Legendary"  
    ];
    $scope.trappingsSelection = $scope.trappingsSelection || [];
    $scope.$watch("trappingsSelection", function(selection) {
        if (typeof selection != 'undefined' && $scope.power) {
            $scope.power.trappings = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.power.trappings.push(collectionItem);
            });
        }
    });
    
    $scope.get();
});