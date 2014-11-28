'use strict';

angular.module('savageWorlds',['ngRoute','ngResource'])
  .config(['$routeProvider', function($routeProvider) {
    $routeProvider
      .when('/',{templateUrl:'views/landing.html',controller:'LandingPageController'})
      .when('/EdgeDescriptions',{templateUrl:'views/EdgeDescription/search.html',controller:'SearchEdgeDescriptionController'})
      .when('/EdgeDescriptions/new',{templateUrl:'views/EdgeDescription/detail.html',controller:'NewEdgeDescriptionController'})
      .when('/EdgeDescriptions/edit/:EdgeDescriptionId',{templateUrl:'views/EdgeDescription/detail.html',controller:'EditEdgeDescriptionController'})
      .when('/EdgeTypes',{templateUrl:'views/EdgeType/search.html',controller:'SearchEdgeTypeController'})
      .when('/EdgeTypes/new',{templateUrl:'views/EdgeType/detail.html',controller:'NewEdgeTypeController'})
      .when('/EdgeTypes/edit/:EdgeTypeId',{templateUrl:'views/EdgeType/detail.html',controller:'EditEdgeTypeController'})
      .when('/GearDescriptions',{templateUrl:'views/GearDescription/search.html',controller:'SearchGearDescriptionController'})
      .when('/GearDescriptions/new',{templateUrl:'views/GearDescription/detail.html',controller:'NewGearDescriptionController'})
      .when('/GearDescriptions/edit/:GearDescriptionId',{templateUrl:'views/GearDescription/detail.html',controller:'EditGearDescriptionController'})
      .when('/HindranceDescriptions',{templateUrl:'views/HindranceDescription/search.html',controller:'SearchHindranceDescriptionController'})
      .when('/HindranceDescriptions/new',{templateUrl:'views/HindranceDescription/detail.html',controller:'NewHindranceDescriptionController'})
      .when('/HindranceDescriptions/edit/:HindranceDescriptionId',{templateUrl:'views/HindranceDescription/detail.html',controller:'EditHindranceDescriptionController'})
      .when('/Members',{templateUrl:'views/Member/search.html',controller:'SearchMemberController'})
      .when('/Members/new',{templateUrl:'views/Member/detail.html',controller:'NewMemberController'})
      .when('/Members/edit/:MemberId',{templateUrl:'views/Member/detail.html',controller:'EditMemberController'})
      .when('/Powers',{templateUrl:'views/Power/search.html',controller:'SearchPowerController'})
      .when('/Powers/new',{templateUrl:'views/Power/detail.html',controller:'NewPowerController'})
      .when('/Powers/edit/:PowerId',{templateUrl:'views/Power/detail.html',controller:'EditPowerController'})
      .when('/Skills',{templateUrl:'views/Skill/search.html',controller:'SearchSkillController'})
      .when('/Skills/new',{templateUrl:'views/Skill/detail.html',controller:'NewSkillController'})
      .when('/Skills/edit/:SkillId',{templateUrl:'views/Skill/detail.html',controller:'EditSkillController'})
      .when('/SkillDescriptions',{templateUrl:'views/SkillDescription/search.html',controller:'SearchSkillDescriptionController'})
      .when('/SkillDescriptions/new',{templateUrl:'views/SkillDescription/detail.html',controller:'NewSkillDescriptionController'})
      .when('/SkillDescriptions/edit/:SkillDescriptionId',{templateUrl:'views/SkillDescription/detail.html',controller:'EditSkillDescriptionController'})
      .when('/Trappings',{templateUrl:'views/Trapping/search.html',controller:'SearchTrappingController'})
      .when('/Trappings/new',{templateUrl:'views/Trapping/detail.html',controller:'NewTrappingController'})
      .when('/Trappings/edit/:TrappingId',{templateUrl:'views/Trapping/detail.html',controller:'EditTrappingController'})
      .otherwise({
        redirectTo: '/'
      });
  }])
  .controller('LandingPageController', function LandingPageController() {
  })
  .controller('NavController', function NavController($scope, $location) {
    $scope.matchesRoute = function(route) {
        var path = $location.path();
        return (path === ("/" + route) || path.indexOf("/" + route + "/") == 0);
    };
  });
