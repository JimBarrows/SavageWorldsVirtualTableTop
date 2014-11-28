angular.module('savageWorlds').factory('EdgeDescriptionResource', function($resource){
    var resource = $resource('rest/edgedescriptions/:EdgeDescriptionId',{EdgeDescriptionId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});