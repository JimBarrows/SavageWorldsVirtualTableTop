import React from 'react';
import PropTypes from 'prop-types';
import { extractPlacesFromStory } from '../../../utils/placeExtractor';

/**
 * PlacesAppendix Component
 * 
 * Domain Context: Plot Point bounded context
 * Business Purpose: Provides Game Masters with a comprehensive view of all places mentioned in a story
 * 
 * Following DDD principles:
 * - Uses ubiquitous language: Story, Places, Game Master
 * - Contains domain logic for place extraction and presentation
 * - Maintains business rules around place uniqueness and sorting
 */
export default class PlacesAppendix extends React.Component {
  static propTypes = {
    storyText: PropTypes.string
  };

  static defaultProps = {
    storyText: ''
  };

  render() {
    const { storyText } = this.props;
    
    // Extract places using domain-specific business logic
    const places = extractPlacesFromStory(storyText);
    
    return (
      <div data-testid="places-appendix-section" className="places-appendix mt-4">
        <h4>Places Appendix</h4>
        
        {places.length > 0 ? (
          <ul data-testid="places-appendix-list" className="list-group">
            {places.map((place, index) => (
              <li 
                key={index}
                data-testid="place-item"
                className="list-group-item"
              >
                {place}
              </li>
            ))}
          </ul>
        ) : (
          <div data-testid="places-appendix-content" className="text-muted">
            No places mentioned in this story
          </div>
        )}
      </div>
    );
  }
}