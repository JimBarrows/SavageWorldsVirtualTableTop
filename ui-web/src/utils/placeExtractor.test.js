import { extractPlacesFromStory } from './placeExtractor';

describe('placeExtractor utility', () => {
  describe('extractPlacesFromStory function', () => {
    test('should extract multiple unique places from story text', () => {
      const storyText = `
        The heroes start their journey in the bustling city of Waterdeep. 
        They must travel through the dangerous Sword Coast to reach the ancient ruins of Undermountain.
        Along the way, they will stop at the peaceful village of Red Larch before facing the perils of the Underdark.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toEqual(['Red Larch', 'Sword Coast', 'Underdark', 'Undermountain', 'Waterdeep']);
    });

    test('should return empty array for story with no place references', () => {
      const storyText = `
        The heroes must solve a complex riddle that tests their wisdom and intelligence.
        They need to think carefully about the clues provided.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toEqual([]);
    });

    test('should remove duplicate places case-insensitively', () => {
      const storyText = `
        The adventure begins in Neverwinter. The heroes explore Neverwinter's districts.
        After leaving Neverwinter, they return to neverwinter for supplies.
        Later they visit NEVERWINTER again.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toEqual(['Neverwinter']);
    });

    test('should handle null or undefined story text', () => {
      expect(extractPlacesFromStory(null)).toEqual([]);
      expect(extractPlacesFromStory(undefined)).toEqual([]);
    });

    test('should handle empty or whitespace-only story text', () => {
      expect(extractPlacesFromStory('')).toEqual([]);
      expect(extractPlacesFromStory('   \n  \t  ')).toEqual([]);
    });

    test('should extract complex multi-word place names', () => {
      const storyText = `
        The expedition explores the Caverns of the Lost King and the Valley of the Shadow.
        They discover the Temple of the Moon Goddess near the River of Souls.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toContain('Caverns of the Lost King');
      expect(places).toContain('Valley of the Shadow');
      expect(places).toContain('Temple of the Moon Goddess');
      expect(places).toContain('River of Souls');
    });

    test('should not extract common capitalized words that are not places', () => {
      const storyText = `
        The heroes start their Quest in January. The Ancient Artifact must be found.
        They travel during Winter and arrive in Spring.
        The Mysterious Stranger helps them find the Magic Sword.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      // Should not include common words, months, or generic fantasy terms
      expect(places).not.toContain('Quest');
      expect(places).not.toContain('January');
      expect(places).not.toContain('Winter');
      expect(places).not.toContain('Spring');
      expect(places).not.toContain('Ancient Artifact');
      expect(places).not.toContain('Mysterious Stranger');
      expect(places).not.toContain('Magic Sword');
    });

    test('should extract places with apostrophes and hyphens correctly', () => {
      const storyText = `
        The journey takes them to Baldur's Gate and the Well-of-Dragons.
        They visit King's Landing and the Storm-Herald's Tower.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toContain("Baldur's Gate");
      expect(places).toContain('Well-of-Dragons');
      expect(places).toContain("King's Landing");
      expect(places).toContain("Storm-Herald's Tower");
    });

    test('should return places sorted alphabetically', () => {
      const storyText = 'They visit Zhentil Keep, then Baldur\'s Gate, and finally Candlekeep.';
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toEqual(["Baldur's Gate", 'Candlekeep', 'Zhentil Keep']);
    });

    test('should handle very long story text efficiently', () => {
      const longStoryText = `
        The epic saga begins in Waterdeep. ${'The heroes travel far and wide. '.repeat(100)}
        Eventually they reach Neverwinter. ${'They face many challenges. '.repeat(100)}
        Finally they arrive at Candlekeep.
      `;
      
      const places = extractPlacesFromStory(longStoryText);
      
      expect(places).toEqual(['Candlekeep', 'Neverwinter', 'Waterdeep']);
    });

    test('should maintain business rule: each place appears only once', () => {
      const storyText = `
        In Waterdeep, the heroes start their quest. Waterdeep is a busy city.
        They leave Waterdeep for adventure. Upon returning to Waterdeep, they rest.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toEqual(['Waterdeep']);
      expect(places.filter(place => place === 'Waterdeep')).toHaveLength(1);
    });
  });

  describe('place pattern recognition', () => {
    test('should recognize geographical features as places', () => {
      const storyText = `
        They cross the Moonshae Isles and climb the Spine of the World.
        The Desert of Anauroch stretches before them.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toContain('Moonshae Isles');
      expect(places).toContain('Spine of the World');
      expect(places).toContain('Desert of Anauroch');
    });

    test('should recognize settlements and structures as places', () => {
      const storyText = `
        From the village of Phandalin to the great city of Neverwinter,
        past the ruins of Thundertree and the manor of Tresendar.
      `;
      
      const places = extractPlacesFromStory(storyText);
      
      expect(places).toContain('Phandalin');
      expect(places).toContain('Neverwinter');
      expect(places).toContain('Thundertree');
      expect(places).toContain('Tresendar');
    });
  });
});