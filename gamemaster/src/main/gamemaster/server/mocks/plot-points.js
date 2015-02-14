module.exports = function(app) {
  var express = require('express');
  var plotPointsRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'First Plot Point', description:'<p>Test Me!</p>'}]

  plotPointsRouter.get('/', function(req, res) {
    res.send({
      'plotPoints': data
    });
  });

  plotPointsRouter.post('/', function(req, res) {
    var newRec = {
      id: data.length + 1,
      name: req.body.plotPoint.name,
      description: req.body.plotPoint.description
    };
    data.push( newRec);
    res.status(201).send({ plotPoint: newRec}).end();
  });

  plotPointsRouter.get('/:id', function(req, res) {
    res.send({
      'plot-points': data[req.params.id -1]
    });
  });

  plotPointsRouter.put('/:id', function(req, res) {
    var plotPoint = data[req.params.id -1];
    plotPoint.name = req.body.plotPoint.name;
    plotPoint.description = req.body.plotPoint.description;
    res.send({
      'plot-points': plotPoint
    });
  });

  plotPointsRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/plotPoints', plotPointsRouter);
};
