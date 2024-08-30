package scala.meta.pc;

import java.util.List;
import java.util.Collections;
import org.eclipse.lsp4j.Location;
import java.util.Optional;

public interface PcSymbolInformation {
  String symbol();
	Optional<Location> definition();  PcSymbolKind kind();
  List<String> parents();
  String dealiasedSymbol();
  String classOwner();
  List<String> overriddenSymbols();
  // overloaded methods
  List<String> alternativeSymbols();
  List<PcSymbolProperty> properties();
  default List<String> recursiveParents() {
    return Collections.emptyList();
  }
  
  default List<String> annotations() {
    return Collections.emptyList();
  }

  default List<String> memberDefsAnnotations() {
    return Collections.emptyList();
  }
}
