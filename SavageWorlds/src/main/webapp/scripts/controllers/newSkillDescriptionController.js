
angular.module('savageWorlds').controller('NewSkillDescriptionController', function ($scope, $location, locationParser, SkillDescriptionResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.skillDescription = $scope.skillDescription || {};
    
    $scope.attributeList = [
        "Agility",
        "Smarts",
        "Strength",
        "Spirit",
        "Vigor"
    ];
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/SkillDescriptions/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        SkillDescriptionResource.save($scope.skillDescription, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/SkillDescriptions");
    };
});