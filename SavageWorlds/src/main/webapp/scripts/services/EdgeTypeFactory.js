angular.module('savageWorlds').factory('EdgeTypeResource', function($resource){
    var resource = $resource('rest/edgetypes/:EdgeTypeId',{EdgeTypeId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});