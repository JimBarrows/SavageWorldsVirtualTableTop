

angular.module('savageWorlds').controller('EditSkillController', function($scope, $routeParams, $location, SkillResource , SkillDescriptionResource) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.skill = new SkillResource(self.original);
            SkillDescriptionResource.queryAll(function(items) {
                $scope.skillSelectionList = $.map(items, function(item) {
                    var wrappedObject = {
                        id : item.id
                    };
                    var labelObject = {
                        value : item.id,
                        text : item.name
                    };
                    if($scope.skill.skill && item.id == $scope.skill.skill.id) {
                        $scope.skillSelection = labelObject;
                        $scope.skill.skill = wrappedObject;
                        self.original.skill = $scope.skill.skill;
                    }
                    return labelObject;
                });
            });
        };
        var errorCallback = function() {
            $location.path("/Skills");
        };
        SkillResource.get({SkillId:$routeParams.SkillId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.skill);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.skill.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/Skills");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/Skills");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.skill.$remove(successCallback, errorCallback);
    };
    
    $scope.$watch("skillSelection", function(selection) {
        if (typeof selection != 'undefined') {
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
    
    $scope.get();
});