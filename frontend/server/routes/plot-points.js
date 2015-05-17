module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var plotPointsRouter = express.Router();
  var bodyParser = require('body-parser');

  plotPointsRouter.get('/', function(req, res) {
    res.send({
      'plot-points': data.plotPoints,
      'races': data.races,
      'archetypes': data.archetypes,
      // 'edges': data.edges,
      // 'gear': data.gear,
      // 'hindrances': data.hindrances,
      // 'places': data.places,
      
      // 'skill-descriptions': data.skills,
      // 'characters': data.characters,
      // 'extras': data.extras,
      // 'beasts': data.beasts,
      // 'powers': data.powers

    });
  });

  plotPointsRouter.post('/', function(req, res) {
    var newRec = req.body.plotPoint;
    newRec.id = data.plotPoints.length + 1,      
    data.plotPoints.push( newRec);
    res.status(201).send({ plotPoint: newRec}).end();
  });

  plotPointsRouter.get('/:id', function(req, res) {
    res.send({
      'plot-points': data.plotPoints[req.params.id -1],
      'archetypes': data.archetypes,
      'races': data.races,
      // 'edges': data.edges,
      // 'gear': data.gear,
      // 'hindrances': data.hindrances,
      // 'places': data.places,
      
      // 'skill-descriptions': data.skills,
      // 'characters': data.characters,
      // 'extras': data.extras,
      // 'beasts': data.beasts,
      // 'powers': data.powers
    });
  });

  plotPointsRouter.put('/:id', function(req, res) {
    var existingRecord = data.plotPoints[req.params.id -1];
    existingRecord = req.body.plotPoint;  
    data.plotPoints[req.params.id - 1] = existingRecord;  
    existingRecord.id = req.params.id;
    data.plotPoints[req.params.id -1] = existingRecord;
    res.send({
      'plot-points': existingRecord,
      'races': data.races,
      'archetypes': data.archetypes,
      // 'edges': data.edges,
      // 'gear': data.gear,
      // 'hindrances': data.hindrances,
      // 'places': data.places,
      
      // 'skill-descriptions': data.skills,
      // 'characters': data.characters,
      // 'extras': data.extras,
      // 'beasts': data.beasts,
      // 'powers': data.powers
    });
  });

  plotPointsRouter.delete('/:id', function(req, res) {
    data.plotPoints.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/plotPoints', plotPointsRouter);
};
