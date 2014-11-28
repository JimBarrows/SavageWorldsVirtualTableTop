

angular.module('savageWorlds').controller('EditSkillDescriptionController', function($scope, $routeParams, $location, SkillDescriptionResource ) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.skillDescription = new SkillDescriptionResource(self.original);
        };
        var errorCallback = function() {
            $location.path("/SkillDescriptions");
        };
        SkillDescriptionResource.get({SkillDescriptionId:$routeParams.SkillDescriptionId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.skillDescription);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.skillDescription.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/SkillDescriptions");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/SkillDescriptions");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.skillDescription.$remove(successCallback, errorCallback);
    };
    
    $scope.attributeList = [
        "Agility",  
        "Smarts",  
        "Strength",  
        "Spirit",  
        "Vigor"  
    ];
    
    $scope.get();
});