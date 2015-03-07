module.exports = function(app) {
  var express = require('express');
  var placeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'Place 1', description:'<p>Place 1 description goes here</p>'},
              {id:2, name:'Place 2', description:'<p>Place 2 description goes here.</p>'}]

  placeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'place': data
    });
  });

  placeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.place;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ place: newRec}).end();
  });

  placeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'place':data[req.params.id -1]
    });
  });

  placeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.placeDescription;  
    existingRecord.id = req.params.id;
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
