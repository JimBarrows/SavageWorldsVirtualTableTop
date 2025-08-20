import React from 'react';
import { render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import PlacesAppendix from './PlacesAppendix';

describe('PlacesAppendix Component', () => {
  describe('extracting places from story text', () => {
    test('should extract multiple unique places from story text', () => {
      const storyText = `
        The heroes start their journey in the bustling city of Waterdeep. 
        They must travel through the dangerous Sword Coast to reach the ancient ruins of Undermountain.
        Along the way, they will stop at the peaceful village of Red Larch before facing the perils of the Underdark.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByText('Places Appendix')).toBeInTheDocument();
      
      // Verify all places are listed
      expect(screen.getByText('Waterdeep')).toBeInTheDocument();
      expect(screen.getByText('Sword Coast')).toBeInTheDocument();
      expect(screen.getByText('Undermountain')).toBeInTheDocument();
      expect(screen.getByText('Red Larch')).toBeInTheDocument();
      expect(screen.getByText('Underdark')).toBeInTheDocument();
    });

    test('should handle story with no place references', () => {
      const storyText = `
        The heroes must solve a complex riddle that tests their wisdom and intelligence.
        They need to think carefully about the clues provided.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByText('Places Appendix')).toBeInTheDocument();
      expect(screen.getByText('No places mentioned in this story')).toBeInTheDocument();
    });

    test('should remove duplicate places case-insensitively', () => {
      const storyText = `
        The adventure begins in Neverwinter. The heroes explore Neverwinter's districts.
        After leaving Neverwinter, they return to neverwinter for supplies.
        Later they visit NEVERWINTER again.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      
      // Should only show Neverwinter once
      const neverwinterElements = screen.getAllByText('Neverwinter');
      expect(neverwinterElements).toHaveLength(1);
    });

    test('should handle empty or null story text', () => {
      render(<PlacesAppendix storyText={null} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByText('No places mentioned in this story')).toBeInTheDocument();
    });

    test('should handle story text with only whitespace', () => {
      render(<PlacesAppendix storyText={"   \n  \t  "} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByText('No places mentioned in this story')).toBeInTheDocument();
    });
  });

  describe('place extraction algorithm', () => {
    test('should extract capitalized words that could be place names', () => {
      const storyText = `
        The journey starts in Baldur's Gate and continues to the Forest of Mir.
        They pass through Castle Ravenloft before reaching the Tower of High Sorcery.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByText("Baldur's Gate")).toBeInTheDocument();
      expect(screen.getByText('Forest of Mir')).toBeInTheDocument();
      expect(screen.getByText('Castle Ravenloft')).toBeInTheDocument();
      expect(screen.getByText('Tower of High Sorcery')).toBeInTheDocument();
    });

    test('should not extract common words that happen to be capitalized', () => {
      const storyText = `
        The heroes start their Quest in January. They must find the Ancient Artifact.
        The journey takes them to Mystical Valley in the month of February.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      // Should extract likely place names
      expect(screen.getByText('Mystical Valley')).toBeInTheDocument();
      
      // Should not extract common words or months
      expect(screen.queryByText('Quest')).not.toBeInTheDocument();
      expect(screen.queryByText('January')).not.toBeInTheDocument();
      expect(screen.queryByText('February')).not.toBeInTheDocument();
      expect(screen.queryByText('Ancient Artifact')).not.toBeInTheDocument();
    });

    test('should handle multi-word place names correctly', () => {
      const storyText = `
        The expedition explores the Caverns of the Lost King and the Valley of the Shadow.
        They discover the Temple of the Moon Goddess near the River of Souls.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByText('Caverns of the Lost King')).toBeInTheDocument();
      expect(screen.getByText('Valley of the Shadow')).toBeInTheDocument();
      expect(screen.getByText('Temple of the Moon Goddess')).toBeInTheDocument();
      expect(screen.getByText('River of Souls')).toBeInTheDocument();
    });
  });

  describe('component rendering', () => {
    test('should render places list with proper test ids', () => {
      const storyText = 'The heroes visit Waterdeep and Neverwinter.';
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByTestId('places-appendix-list')).toBeInTheDocument();
      
      const placeItems = screen.getAllByTestId('place-item');
      expect(placeItems).toHaveLength(2);
    });

    test('should render no places message with proper test id', () => {
      const storyText = 'A simple story with no places.';
      
      render(<PlacesAppendix storyText={storyText} />);
      
      expect(screen.getByTestId('places-appendix-section')).toBeInTheDocument();
      expect(screen.getByTestId('places-appendix-content')).toBeInTheDocument();
      expect(screen.getByText('No places mentioned in this story')).toBeInTheDocument();
    });
  });

  describe('business rules validation', () => {
    test('should support Game Master workflow for story location tracking', () => {
      const storyText = `
        The party begins in the trade city of Suzail and travels to the mystical Myth Drannor.
        Their quest leads them through the Cormanthor Forest.
      `;
      
      render(<PlacesAppendix storyText={storyText} />);
      
      // Verify business requirement: Game Master can see all locations
      expect(screen.getByText('Places Appendix')).toBeInTheDocument();
      expect(screen.getByText('Suzail')).toBeInTheDocument();
      expect(screen.getByText('Myth Drannor')).toBeInTheDocument();
      expect(screen.getByText('Cormanthor Forest')).toBeInTheDocument();
    });

    test('should provide alphabetically sorted place list for easy reference', () => {
      const storyText = 'They visit Zhentil Keep, then Baldur\'s Gate, and finally Candlekeep.';
      
      render(<PlacesAppendix storyText={storyText} />);
      
      const placeItems = screen.getAllByTestId('place-item');
      const placeNames = placeItems.map(item => item.textContent);
      
      // Should be sorted alphabetically for Game Master convenience
      expect(placeNames).toEqual(["Baldur's Gate", 'Candlekeep', 'Zhentil Keep']);
    });
  });
});