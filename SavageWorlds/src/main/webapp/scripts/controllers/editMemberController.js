

angular.module('savageWorlds').controller('EditMemberController', function($scope, $routeParams, $location, MemberResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.member = new MemberResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/Members");
        };
        MemberResource.get({MemberId:$routeParams.MemberId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.member);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.member.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/Members");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/Members");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.member.$remove(successCallback, errorCallback);
    };
    
    
    $scope.get();
});