angular.module('savageWorlds').factory('SkillDescriptionResource', function($resource){
    var resource = $resource('rest/skilldescriptions/:SkillDescriptionId',{SkillDescriptionId:'@id'},{'queryAll':{method:'GET',isArray:true},'query':{method:'GET',isArray:false},'update':{method:'PUT'}});
    return resource;
});