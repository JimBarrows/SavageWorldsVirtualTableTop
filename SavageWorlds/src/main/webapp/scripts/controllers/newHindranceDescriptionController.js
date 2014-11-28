
angular.module('savageWorlds').controller('NewHindranceDescriptionController', function ($scope, $location, locationParser, HindranceDescriptionResource ) {
    $scope.disabled = false;
    $scope.$location = $location;
    $scope.hindranceDescription = $scope.hindranceDescription || {};
    
    $scope.severityList = [
        "Minor",
        "Major",
        "Either"
    ];
    

    $scope.save = function() {
        var successCallback = function(data,responseHeaders){
            var id = locationParser(responseHeaders);
            $location.path('/HindranceDescriptions/edit/' + id);
            $scope.displayError = false;
        };
        var errorCallback = function() {
            $scope.displayError = true;
        };
        HindranceDescriptionResource.save($scope.hindranceDescription, successCallback, errorCallback);
    };
    
    $scope.cancel = function() {
        $location.path("/HindranceDescriptions");
    };
});