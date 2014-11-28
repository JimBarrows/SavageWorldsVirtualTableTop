
angular.module('savageWorlds').controller('NewTrappingController', function ($scope, $location, locationParser, TrappingResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.trapping = $scope.trapping || {};
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/Trappings/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        TrappingResource.save($scope.trapping, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/Trappings");
    };
});