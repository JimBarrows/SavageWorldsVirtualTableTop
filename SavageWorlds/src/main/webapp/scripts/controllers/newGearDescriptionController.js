
angular.module('savageWorlds').controller('NewGearDescriptionController', function ($scope, $location, locationParser, GearDescriptionResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.gearDescription = $scope.gearDescription || {};
    
    $scope.eraList = [
        "Medieval",
        "BlackPowder",
        "Modern",
        "Futuristic"
    ];
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/GearDescriptions/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        GearDescriptionResource.save($scope.gearDescription, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/GearDescriptions");
    };
});