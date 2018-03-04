import gql from 'graphql-tag'

var {Given, Then, When} = require('cucumber')

Given('I am ready to create a plot point', function (callback) {
  this.plot_point = {}
  callback()
})

Given('I have provided a plot point name of {string}', function (string, callback) {
  this.plot_point.name = string
  callback()
})

Given('I have provided a plot point description of {string}', function (string, callback) {
  this.plot_point.description = string
  callback()
})

Given('I have provided a plot point brief description of {string}', function (string, callback) {
  this.plot_point.brief_description = string
  callback()
})

When('I save the plot point', function () {

  let {name, description, brief_description} = this.plot_point
  return this.plotPointService.mutate({
                                        mutation: gql`mutation plotPointCreate($plotPoint: InputPlotPoint) {plot_point_create(plotPoint: $plotPoint)}`,
                                        variables:
                                          {
                                            plotPoint: {
                                              name,
                                              description,
                                              brief_description
                                            }
                                          }
                                      })
  .then(data => this.result.data = data)
  .catch(e => this.result.error = e)
})

Then('the save was successful', function (callback) {
  expect(this.result.error).to.be.not.ok
  expect(this.result.data).to.be.ok
  callback()
})

Then('the plot point is in the database', async function () {
  this.database_plot_point = await this.swga_db.one('select id, name, description, brief_description from plot_points where id = ${plot_point_create}', this.result.data.data)
  expect(this.database_plot_point).to.be.ok
})

Then('the plot point name  is {string}', function (string, callback) {
  expect(this.database_plot_point.name).to.be.equal(string)
  callback()
})

Then('the plot point description is {string}', function (string, callback) {
  expect(this.database_plot_point.description).to.be.equal(string)
  callback()
})