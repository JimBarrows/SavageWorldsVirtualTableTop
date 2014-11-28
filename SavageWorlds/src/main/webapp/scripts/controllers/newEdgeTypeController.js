
angular.module('savageWorlds').controller('NewEdgeTypeController', function ($scope, $location, locationParser, EdgeTypeResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.edgeType = $scope.edgeType || {};
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/EdgeTypes/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        EdgeTypeResource.save($scope.edgeType, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/EdgeTypes");
    };
});