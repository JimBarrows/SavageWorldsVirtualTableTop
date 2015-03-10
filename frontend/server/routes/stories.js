module.exports = function(app) {
  var express = require('express');
  var storyRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.stories;

  storyRouter.get('/', function(req, res) {
    res.send({
      'story': data
    });
  });

  storyRouter.post('/', function(req, res) {
    var newRec = req.body.story;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ story: newRec}).end();
  });

  storyRouter.get('/:id', function(req, res) {
    res.send({
      'story':data[req.params.id -1]
    });
  });

  storyRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.story;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'story': existingRecord
    });
  });

  storyRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/stories', storyRouter);
};
