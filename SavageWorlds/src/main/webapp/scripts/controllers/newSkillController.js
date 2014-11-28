
angular.module('savageWorlds').controller('NewSkillController', function ($scope, $location, locationParser, SkillResource , SkillDescriptionResource) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.skill = $scope.skill || {};
    
    $scope.skillList = SkillDescriptionResource.queryAll(function(items){
        $scope.skillSelectionList = $.map(items, function(item) {
            return ( {
                value : item.id,
                text : item.name
            });
        });
    });
    $scope.$watch("skillSelection", function(selection) {
        if ( typeof selection != 'undefined') {
            $scope.skill.skill = {};
            $scope.skill.skill.id = selection.value;
        }
    });
    
    $scope.diceList = [
        "d4",
        "d6",
        "d8",
        "d10",
        "d12"
    ];
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/Skills/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        SkillResource.save($scope.skill, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/Skills");
    };
});