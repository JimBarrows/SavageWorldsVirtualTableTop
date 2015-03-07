
module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var placeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');

  placeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'place': data.places
    });
  });

  placeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.place;
    newRec.id = data.places.length + 1,      
    data.places.push( newRec);
    res.status(201).send({ place: newRec}).end();
  });

  placeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'place':data.places[req.params.id -1]
    });
  });

  placeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data.places[req.params.id -1];
    existingRecord = req.body.placeDescription;  
    existingRecord.id = req.params.id;
    data.places[req.params.id -1] = existingRecord;
    res.send({
      'place': existingRecord
    });
  });

  placeDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/places', placeDescriptionRouter);
};
