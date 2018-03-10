import React, {Component} from 'react'
import {Route, Switch} from 'react-router-dom'
import './App.css'
import Header from './components/Header'
import PlotPointList from './components/PlotPointList'

class App extends Component {
  render () {
    return (
      <div >
        <Header />
        {/*<MessageDisplay id={'application'} />*/}
        <div id={"layout"} className="container" role={"main"} >
          <Switch >
            <Route exact path="/" component={PlotPointList} />
          </Switch >
        </div >
      </div >
    )
  }
}

export default App
