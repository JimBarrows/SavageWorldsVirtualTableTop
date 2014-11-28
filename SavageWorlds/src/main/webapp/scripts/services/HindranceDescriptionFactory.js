angular.module('savageWorlds').factory('HindranceDescriptionResource', function($resource){
    var resource = $resource('rest/hindrancedescriptions/:HindranceDescriptionId',{HindranceDescriptionId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});