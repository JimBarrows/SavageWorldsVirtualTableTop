import PropTypes from 'prop-types';
import React from 'react';

export default class DramaticPersonae extends React.Component {
  static defaultProps = {
    characters: []
  }

  static propTypes = {
    id: PropTypes.string.isRequired,
    characters: PropTypes.array
  }

  renderCharacterSummary = (character, index) => {
    return (
      <li key={index} 
          className="list-group-item" 
          data-testid={`character-summary-${index}`}>
        <div className="character-summary">
          <h5 className="character-name mb-2">{character.name || 'Unnamed Character'}</h5>
          {character.description && (
            <p className="character-description text-muted mb-1">
              <em>{character.description}</em>
            </p>
          )}
          {character.background && (
            <span className="badge bg-secondary character-role">
              {character.background}
            </span>
          )}
        </div>
      </li>
    );
  }

  renderEmptyState = () => {
    return (
      <div className="text-center text-muted py-4">
        <p className="mb-3">No characters added to this plot point</p>
        <p className="small">Characters will appear here once added to the story.</p>
      </div>
    );
  }

  renderCharactersList = () => {
    const { characters } = this.props;
    
    if (!characters || characters.length === 0) {
      return this.renderEmptyState();
    }

    return (
      <ul className="list-group list-group-flush" role="list">
        {characters.map((character, index) => 
          this.renderCharacterSummary(character, index)
        )}
      </ul>
    );
  }

  render() {
    const { id } = this.props;
    const componentId = `DramaticPersonae-${id}`;

    return (
      <div id={componentId} 
           className="dramatic-personae-appendix" 
           data-testid="dramatic-personae-appendix">
        
        <div className="d-flex justify-content-between align-items-center mb-4">
          <h2 className="appendix-title mb-0">Dramatic Personae</h2>
          <a href="#Characters" 
             className="btn btn-outline-primary btn-sm"
             onClick={(e) => {
               e.preventDefault();
               // This will be handled by parent component navigation
               if (this.props.onNavigateToCharacters) {
                 this.props.onNavigateToCharacters();
               }
             }}>
            Edit Characters
          </a>
        </div>

        <div className="appendix-content">
          <div className="row">
            <div className="col-12">
              <div className="card">
                <div className="card-header">
                  <h6 className="card-title mb-0">
                    Characters in this Story
                    {this.props.characters && this.props.characters.length > 0 && (
                      <span className="badge bg-primary ms-2">
                        {this.props.characters.length}
                      </span>
                    )}
                  </h6>
                </div>
                <div className="card-body p-0">
                  {this.renderCharactersList()}
                </div>
              </div>
            </div>
          </div>
          
          <div className="row mt-3">
            <div className="col-12">
              <div className="alert alert-info" role="alert">
                <strong>Quick Reference:</strong> This appendix provides a summary of all dramatic personae 
                (characters) in your plot point for easy reference during gameplay. 
                To add, edit, or remove characters, use the Characters section in the navigation menu.
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

DramaticPersonae.propTypes = {
  id: PropTypes.string.isRequired,
  characters: PropTypes.array,
  onNavigateToCharacters: PropTypes.func
};