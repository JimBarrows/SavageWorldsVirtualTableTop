package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.Story;
import org.springframework.data.repository.PagingAndSortingRepository;

import java.util.Set;

public interface SavageTalesRepository extends PagingAndSortingRepository<Story, Long> {

	Set<Story> findByName(String name);
}
