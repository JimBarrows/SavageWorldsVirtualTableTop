
angular.module('savageWorlds').controller('NewMemberController', function ($scope, $location, locationParser, MemberResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.member = $scope.member || {};
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/Members/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        MemberResource.save($scope.member, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/Members");
    };
});