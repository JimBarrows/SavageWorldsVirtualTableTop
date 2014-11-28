

angular.module('savageWorlds').controller('EditHindranceDescriptionController', function($scope, $routeParams, $location, HindranceDescriptionResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.hindranceDescription = new HindranceDescriptionResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/HindranceDescriptions");
        };
        HindranceDescriptionResource.get({HindranceDescriptionId:$routeParams.HindranceDescriptionId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.hindranceDescription);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.hindranceDescription.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/HindranceDescriptions");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/HindranceDescriptions");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.hindranceDescription.$remove(successCallback, errorCallback);
    };
    
    $scope.severityList = [
        "Minor",  
        "Major",  
        "Either"  
    ];
    
    $scope.get();
});