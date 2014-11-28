

angular.module('savageWorlds').controller('EditEdgeDescriptionController', function($scope, $routeParams, $location, EdgeDescriptionResource , SkillResource, EdgeDescriptionResource, EdgeTypeResource) {
    var self = this;
    $scope.disabled = false;
    $scope.$location = $location;
    
    $scope.get = function() {
        var successCallback = function(data){
            self.original = data;
            $scope.edgeDescription = new EdgeDescriptionResource(self.original);
            SkillResource.queryAll(function(items) {
                $scope.minimumSkillsSelectionList = $.map(items, function(item) {
                    var wrappedObject = {
                        id : item.id
                    };
                    var labelObject = {
                        value : item.id,
                        text : item.dice
                    };
                    if($scope.edgeDescription.minimumSkills){
                        $.each($scope.edgeDescription.minimumSkills, function(idx, element) {
                            if(item.id == element.id) {
                                $scope.minimumSkillsSelection.push(labelObject);
                                $scope.edgeDescription.minimumSkills.push(wrappedObject);
                            }
                        });
                        self.original.minimumSkills = $scope.edgeDescription.minimumSkills;
                    }
                    return labelObject;
                });
            });
            EdgeDescriptionResource.queryAll(function(items) {
                $scope.requiredEdgesSelectionList = $.map(items, function(item) {
                    var wrappedObject = {
                        id : item.id
                    };
                    var labelObject = {
                        value : item.id,
                        text : item.name
                    };
                    if($scope.edgeDescription.requiredEdges){
                        $.each($scope.edgeDescription.requiredEdges, function(idx, element) {
                            if(item.id == element.id) {
                                $scope.requiredEdgesSelection.push(labelObject);
                                $scope.edgeDescription.requiredEdges.push(wrappedObject);
                            }
                        });
                        self.original.requiredEdges = $scope.edgeDescription.requiredEdges;
                    }
                    return labelObject;
                });
            });
            EdgeTypeResource.queryAll(function(items) {
                $scope.edgeTypeSelectionList = $.map(items, function(item) {
                    var wrappedObject = {
                        id : item.id
                    };
                    var labelObject = {
                        value : item.id,
                        text : item.name
                    };
                    if($scope.edgeDescription.edgeType && item.id == $scope.edgeDescription.edgeType.id) {
                        $scope.edgeTypeSelection = labelObject;
                        $scope.edgeDescription.edgeType = wrappedObject;
                        self.original.edgeType = $scope.edgeDescription.edgeType;
                    }
                    return labelObject;
                });
            });
        };
        var errorCallback = function() {
            $location.path("/EdgeDescriptions");
        };
        EdgeDescriptionResource.get({EdgeDescriptionId:$routeParams.EdgeDescriptionId}, successCallback, errorCallback);
    };

    $scope.isClean = function() {
        return angular.equals(self.original, $scope.edgeDescription);
    };

    $scope.save = function() {
        var successCallback = function(){
            $scope.get();
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        };
        $scope.edgeDescription.$update(successCallback, errorCallback);
    };

    $scope.cancel = function() {
        $location.path("/EdgeDescriptions");
    };

    $scope.remove = function() {
        var successCallback = function() {
            $location.path("/EdgeDescriptions");
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError=true;
        }; 
        $scope.edgeDescription.$remove(successCallback, errorCallback);
    };
    
    $scope.minimumRankList = [
        "Novice",  
        "Seasoned",  
        "Veteran",  
        "Heroic",  
        "Legendary"  
    ];
    $scope.requiredTypeList = [
        "Extra",  
        "WildCard"  
    ];
    $scope.minimumSkillsSelection = $scope.minimumSkillsSelection || [];
    $scope.$watch("minimumSkillsSelection", function(selection) {
        if (typeof selection != 'undefined' && $scope.edgeDescription) {
            $scope.edgeDescription.minimumSkills = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.edgeDescription.minimumSkills.push(collectionItem);
            });
        }
    });
    $scope.requiredEdgesSelection = $scope.requiredEdgesSelection || [];
    $scope.$watch("requiredEdgesSelection", function(selection) {
        if (typeof selection != 'undefined' && $scope.edgeDescription) {
            $scope.edgeDescription.requiredEdges = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.edgeDescription.requiredEdges.push(collectionItem);
            });
        }
    });
    $scope.$watch("edgeTypeSelection", function(selection) {
        if (typeof selection != 'undefined') {
            $scope.edgeDescription.edgeType = {};
            $scope.edgeDescription.edgeType.id = selection.value;
        }
    });
    
    $scope.get();
});