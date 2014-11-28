angular.module('savageWorlds').factory('PowerResource', function($resource){
    var resource = $resource('rest/powers/:PowerId',{PowerId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});