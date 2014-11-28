
angular.module('savageWorlds').controller('NewEdgeDescriptionController', function ($scope, $location, locationParser, EdgeDescriptionResource , SkillResource, EdgeDescriptionResource, EdgeTypeResource) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.edgeDescription = $scope.edgeDescription || {};
    
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
    
    $scope.minimumSkillsList = SkillResource.queryAll(function(items){
        $scope.minimumSkillsSelectionList = $.map(items, function(item) {
            return ( {
                value : item.id,
                text : item.dice
            });
        });
    });
    $scope.$watch("minimumSkillsSelection", function(selection) {
        if (typeof selection != 'undefined') {
            $scope.edgeDescription.minimumSkills = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.edgeDescription.minimumSkills.push(collectionItem);
            });
        }
    });
    
    $scope.requiredEdgesList = EdgeDescriptionResource.queryAll(function(items){
        $scope.requiredEdgesSelectionList = $.map(items, function(item) {
            return ( {
                value : item.id,
                text : item.name
            });
        });
    });
    $scope.$watch("requiredEdgesSelection", function(selection) {
        if (typeof selection != 'undefined') {
            $scope.edgeDescription.requiredEdges = [];
            $.each(selection, function(idx,selectedItem) {
                var collectionItem = {};
                collectionItem.id = selectedItem.value;
                $scope.edgeDescription.requiredEdges.push(collectionItem);
            });
        }
    });
    
    $scope.edgeTypeList = EdgeTypeResource.queryAll(function(items){
        $scope.edgeTypeSelectionList = $.map(items, function(item) {
            return ( {
                value : item.id,
                text : item.name
            });
        });
    });
    $scope.$watch("edgeTypeSelection", function(selection) {
        if ( typeof selection != 'undefined') {
            $scope.edgeDescription.edgeType = {};
            $scope.edgeDescription.edgeType.id = selection.value;
        }
    });
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/EdgeDescriptions/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        EdgeDescriptionResource.save($scope.edgeDescription, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/EdgeDescriptions");
    };
});