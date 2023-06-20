package info.kgeorgiy.ja.khairullin.i18n.stats.bundles;

import java.util.ListResourceBundle;

public class BundleEn extends ListResourceBundle {
    @Override
    protected Object[][] getContents() {
        return new Object[][]{
                {"not_found", "Not found"},
                {"var", "various"},
                {"date_stat", "Statistics by date"},
                {"date_num", "Number of date"},
                {"min_date", "Minimum date"},
                {"max_date", "Maximum date"},
                {"avg_date", "Average date"},
                {"an_file", "Analysing file"},
                {"com_stat", "Common statistics"},
                {"num_stat", "Statistics by number"},
                {"num_num", "Number of numbers"},
                {"min_num", "Minimum number"},
                {"max_num", "Maximum number"},
                {"avg_num", "Average number"},
                {"money_stat", "Statistics by amounts"},
                {"money_num", "Number of amounts"},
                {"min_money", "Minimum amount"},
                {"max_money", "Maximum amount"},
                {"avg_money", "Average amount"},
                {"sen_stat", "Statistics by sentences"},
                {"sen_num", "Number of sentences"},
                {"min_sen", "Minimum sentence"},
                {"max_sen", "Maximum sentence"},
                {"min_sen_len", "Minimum length of sentence"},
                {"max_sen_len", "Maximum length of sentence"},
                {"avg_sen_len", "Average length of sentence"},
                {"word_stat", "Statistics by words"},
                {"word_num", "Number of words"},
                {"min_word", "Minimum word"},
                {"max_word", "Maximum word"},
                {"min_word_len", "Minimum length of word"},
                {"max_word_len", "Maximum length of word"},
                {"avg_word_len", "Average length of word"}
        };
    }
}
