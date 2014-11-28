

angular.module('savageWorlds').controller('EditEdgeTypeController', function($scope, $routeParams, $location, EdgeTypeResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.edgeType = new EdgeTypeResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/EdgeTypes");
        };
        EdgeTypeResource.get({EdgeTypeId:$routeParams.EdgeTypeId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.edgeType);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.edgeType.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/EdgeTypes");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/EdgeTypes");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.edgeType.$remove(successCallback, errorCallback);
    };
    
    
    $scope.get();
});