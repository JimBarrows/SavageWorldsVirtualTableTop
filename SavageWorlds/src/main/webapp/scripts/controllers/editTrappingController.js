

angular.module('savageWorlds').controller('EditTrappingController', function($scope, $routeParams, $location, TrappingResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.trapping = new TrappingResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/Trappings");
        };
        TrappingResource.get({TrappingId:$routeParams.TrappingId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.trapping);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.trapping.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/Trappings");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/Trappings");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.trapping.$remove(successCallback, errorCallback);
    };
    
    
    $scope.get();
});