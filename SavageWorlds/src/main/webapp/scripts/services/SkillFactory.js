angular.module('savageWorlds').factory('SkillResource', function($resource){
    var resource = $resource('rest/skills/:SkillId',{SkillId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});