angular.module('savageWorlds').factory('TrappingResource', function($resource){
    var resource = $resource('rest/trappings/:TrappingId',{TrappingId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});