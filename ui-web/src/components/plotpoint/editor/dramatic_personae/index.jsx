import PropTypes from 'prop-types';
import React from 'react';
import DramaticPersonae from './DramaticPersonae';

export default class DramaticPersonaeContainer extends React.Component {
  static defaultProps = {
    characters: []
  }

  static propTypes = {
    id: PropTypes.string.isRequired,
    characters: PropTypes.array,
    onNavigateToCharacters: PropTypes.func
  }

  render() {
    const { id, characters, onNavigateToCharacters } = this.props;
    
    return (
      <DramaticPersonae
        id={id}
        characters={characters}
        onNavigateToCharacters={onNavigateToCharacters}
      />
    );
  }
}

export { DramaticPersonae };