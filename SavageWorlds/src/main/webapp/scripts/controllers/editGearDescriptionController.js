

angular.module('savageWorlds').controller('EditGearDescriptionController', function($scope, $routeParams, $location, GearDescriptionResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.gearDescription = new GearDescriptionResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/GearDescriptions");
        };
        GearDescriptionResource.get({GearDescriptionId:$routeParams.GearDescriptionId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.gearDescription);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.gearDescription.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/GearDescriptions");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/GearDescriptions");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.gearDescription.$remove(successCallback, errorCallback);
    };
    
    $scope.eraList = [
        "Medieval",  
        "BlackPowder",  
        "Modern",  
        "Futuristic"  
    ];
    
    $scope.get();
});