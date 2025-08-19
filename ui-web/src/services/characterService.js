// Character service for fetching available characters
// This is currently mock data but can be expanded to real API calls

const characterService = {
  // Get available characters for scenes
  async getAvailableCharacters() {
    // Mock data for now - this would normally be an API call
    return [
      { name: 'Sir Gareth', description: 'A noble knight' },
      { name: 'Mara', description: 'A cunning thief' },
      { name: 'Grimjaw', description: 'An orc bartender' },
      { name: 'Eldara', description: 'An elven mage' }
    ];
  }
};

export default characterService;