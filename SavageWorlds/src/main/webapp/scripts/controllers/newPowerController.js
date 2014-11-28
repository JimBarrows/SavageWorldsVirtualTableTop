
angular.module('savageWorlds').controller('NewPowerController', function ($scope, $location, locationParser, PowerResource , TrappingResource) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.power = $scope.power || {};
    
    $scope.rankList = [
        "Novice",
        "Seasoned",
        "Veteran",
        "Heroic",
        "Legendary"
    ];
    
    $scope.trappingsList = TrappingResource.queryAll(function(items){
        $scope.trappingsSelectionList = $.map(items, function(item) {
            return ( {
                value : item.id,
                text : item.name
            });
        });
    });
    $scope.$watch("trappingsSelection", function(selection) {
        if (typeof selection != 'undefined') {
            $scope.power.trappings = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.power.trappings.push(collectionItem);
            });
        }
    });
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/Powers/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        PowerResource.save($scope.power, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/Powers");
    };
});