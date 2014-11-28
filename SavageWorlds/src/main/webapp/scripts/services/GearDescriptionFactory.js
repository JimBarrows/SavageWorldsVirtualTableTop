angular.module('savageWorlds').factory('GearDescriptionResource', function($resource){
    var resource = $resource('rest/geardescriptions/:GearDescriptionId',{GearDescriptionId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});